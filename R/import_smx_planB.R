#' Import data from the Oracle xe-database
#'
#' @param id synaflokkur. Default is set to 30
#' @param gid veidarfaeri. Default is set to 73
#' @param year Current cruise year. If not specificed (default) use current (computer) year.
#' @param store A boolean, if TRUE then the returned returned object is also
#' saved as hafvog.rds in directory data
#'
import_smx_planB <- function(id = 30, gid = 73, year, store = FALSE) {

  schema <- c("FISKAR", "HAFVOG")

  # ----------------------------------------------------------------------------
  # Constants
  if(missing(year)) {
    now.year <- lubridate::now() %>% lubridate::year()
  } else {
    now.year <- year
  }


  con <- DBI::dbConnect(DBI::dbDriver('Oracle'),
                        user = "hafvog_user",
                        password = "hafvog",
                        dbname='xe')


  # ----------------------------------------------------------------------------
  # IMPORT
  print("Lesa ur XE-gagnagrunninum")

  # A. FISKAR ------------------------------------------------------------------
  st.list <- nu.list <- le.list <- kv.list <- list()
  for(i in 1:length(schema)) {

    st <-
      lesa_stodvarB(con, schema[i], synaflokkur = id) %>%
      dplyr::filter(synaflokkur %in% id, veidarfaeri %in% gid)

    if(st %>% count() %>% collect() == 0) {
      stop("There is no data for specified 'synaflokkur' (id) and 'veidarfaeri' (gid)")
    }

    if(schema[i] == "FISKAR") {
      st <-
        st %>%
        dplyr::filter(ar < now.year)
    } else {
      st <-
        st %>%
        dplyr::filter(ar == now.year)
    }

    nu.list[[i]] <-
      st %>%
      dplyr::select(synis_id) %>%
      #dplyr::left_join(lesa_numer(con, schema[i]), by = "synis_id") %>%
      dplyr::left_join(lesa_numerB(con, schema = schema[i])) %>%
      dplyr::select(synis_id, tegund, fj_maelt, fj_talid, fj_alls) %>%
      dplyr::filter(!is.na(tegund))

    le.list[[i]] <-
      st %>%
      dplyr::select(synis_id) %>%
      dplyr::left_join(lesa_lengdirB(con, schema[i]),
                       by = "synis_id") %>%
      dplyr::group_by(synis_id, tegund, lengd) %>%
      dplyr::summarise(fjoldi = sum(fjoldi, na.rm = TRUE),
                       .groups = "drop") %>%
      #dplyr::collect(n = Inf)  %>%
      dplyr::filter(!is.na(tegund))

    kv.list[[i]] <-
      st %>%
      dplyr::select(synis_id) %>%
      dplyr::left_join(lesa_kvarnirB(con, schema[i]), by = "synis_id") #%>%
      #dplyr::collect(n = Inf)

    st.list[[i]] <-
      st %>%
      #dplyr::collect(n = Inf) %>%
      dplyr::mutate(lon1 = -kastad_v_lengd,
                    lat1 = kastad_n_breidd,
                    lon2 = -hift_v_lengd,
                    lat2 = hift_n_breidd) %>%
      geo::geoconvert(col.names = c("lat1", "lon1")) %>%
      geo::geoconvert(col.names = c("lat2", "lon2")) %>%
      dplyr::mutate(lon = (lon1 + lon2) / 2,
                    lat = (lat1 + lat2) / 2,
                    toglengd = ifelse(is.na(toglengd), 4, toglengd))

  }

  st <-
    dplyr::bind_rows(st.list) %>%
    dplyr::mutate(index = reitur * 100 + tognumer)
  nu <- dplyr::bind_rows(nu.list)
  le <- dplyr::bind_rows(le.list)
  kv <- dplyr::bind_rows(kv.list)

  skraning <-
    #tbl_mar(con, "hafvog.stodvar") %>%
    lesa_stodvarB(con, schema = "HAFVOG", synaflokkur = id) %>%
    #dplyr::select(synis_id:heildarafli, synaflokkur) %>%
    dplyr::mutate(ar = lubridate::year(dags)) %>%
    #dplyr::left_join(tbl_mar(con, "hafvog.togstodvar") %>%
    #                   dplyr::select(synis_id:eykt), by = "synis_id") %>%
    #dplyr::left_join(tbl_mar(con, "hafvog.umhverfi") %>%
    #                   dplyr::select(synis_id:sjondypi), by = "synis_id") %>%
    dplyr::mutate(index = reitur * 100 + tognumer) %>%
    dplyr::filter(synaflokkur %in% id,
                  veidarfaeri %in% gid,
                  ar == now.year) %>%
    dplyr::select(synis_id) %>%
    #dplyr::left_join(tbl_mar(con, "hafvog.skraning"),
    #                 by = "synis_id") %>%
    dplyr::left_join(hv_skraningB(con)) %>%
    #dplyr::collect(n = Inf) %>%
    dplyr::mutate(synis_id = -synis_id)

  # ----------------------------------------------------------------------------
  # Other stuff needed from hafvog
  # B. STADLAR -----------------------------------------------------------------

  stadlar.rallstodvar <-
    #lesa_stadla_rallstodvar(con) %>%
    hv_stadla_rallstodvarB(con) %>%
    dplyr::filter(veidarfaeri_id %in% gid,
                  synaflokkur %in% id) %>%
    dplyr::collect(n = Inf) %>%
    # fix an error in hift_v for SMH, should be corrected in database
    dplyr::mutate(hift_v = ifelse(hift_v == -2444550, -244455, hift_v)) %>%
    geo::geoconvert(col.names = c("kastad_v", "kastad_n")) %>%
    geo::geoconvert(col.names = c("hift_v",   "hift_n"))

  lid <- stadlar.rallstodvar$leidangur_id[[1]]
  stadlar.tegundir <-
    #lesa_stadla_tegund_smb(con) %>%
    hv_stadlar_tegundirB(con) %>%
    rename(tegund = fisktegund_id) %>%
    dplyr::filter(leidangur_id == lid) %>%
    dplyr::arrange(tegund) %>%
    #dplyr::collect(n = Inf) %>%
    tidyr::gather(variable, value, lifur_low:kynkirtlar_high) %>%
    dplyr::mutate(value = value / 100) %>%
    tidyr::spread(variable, value)

  stadlar.lw <-
    #lesa_stadla_lw(con) %>%
    hv_lwB(con) %>%
    rename(tegund = fisktegund_id) %>%
    arrange(tegund, lengd) %>%
    #dplyr::collect(n = Inf) %>%
    dplyr::mutate(osl = oslaegt_a * lengd^oslaegt_b,
                  sl = slaegt_a * lengd^slaegt_b) %>%
    dplyr::mutate(osl1 = osl * (1 - fravik),
                  osl2 = osl * (1 + fravik),
                  sl1 = sl * (1 - fravik),
                  sl2 = sl * (1 + fravik)) %>%
    dplyr::select(tegund, lengd, osl1:sl2)

  fisktegundir <-
    #tbl_mar(con, "hafvog.fisktegundir") %>%
    hv_fisktegundirB(con) %>%
    dplyr::select(tegund, heiti) %>%
    dplyr::arrange(tegund) %>%
    dplyr::collect(n = Inf)

  aid <-
    #tbl_mar(con, "hafvog.maeliatridi") %>%
    #dplyr::collect() %>%
    hv_maeliatridiB(con) %>%
    dplyr::rename(aid = id, adgerd = heiti)
  sid <-
    hv_fisktegundirB(con) %>%
    dplyr::select(sid = tegund, tegund = heiti) %>%
    dplyr::arrange(tegund) %>%
    dplyr::collect(n = Inf)
  prey_names <-
    hv_faeduhoparB(con)
  pred <-
    skraning %>%
    filter(!is.na(magaastand)) %>%
    select(synis_id, pred = tegund, nr, oslaegt, slaegt, astand = magaastand) %>%
    left_join(dbGetQuery(con, 'select * from HAFVOG.MAGAASTAND') %>%
                as_tibble() %>%
                janitor::clean_names() %>%
                select(astand, lysing_astands)) %>%
    select(-astand) %>%
    rename(astand = lysing_astands)
  prey <-
    skraning %>%
    filter(maeliadgerd %in% c(20,21)) %>%
    rename(prey = tegund,
           pred = ranfiskurteg,
           pnr = nr,
           nr = kvarnanr) %>%
    select(synis_id, pred, nr, prey, pnr,
           n = fjoldi, lengd, kyn, thyngd = heildarthyngd)

  other.stuff <- list(stadlar.rallstodvar = stadlar.rallstodvar,
                      stadlar.tegundir = stadlar.tegundir,
                      stadlar.lw = stadlar.lw,
                      fisktegundir = fisktegundir,
                      aid = aid,
                      sid = sid,
                      prey_names = prey_names,
                      prey = prey,
                      pred = pred)



  ret <- list(st = st, nu = nu, le = le, kv = kv, skraning = skraning,
              other.stuff = other.stuff)

  if(store) {

    if(!dir.exists("data2")) dir.create("data2")
    ret %>% readr::write_rds(path = "data2/hafvog.rds")
  }

  return(ret)
}


