#' @title munge_for_smxapp
#'
#' @description Create R-binary bundle for the smxapp
#'
#' @param res a list contain st, nu, le and kv and other things. The object is
#' generated via function import_smx (in the xe-package)
#' @param cruise leidangrar in schema hafvog
#' @param rda.file name the exported binary file (default smb_dashboard.rda)
#' which is stored in the directory data2
#'
#' @export
#'
munge_for_smxapp <- function(res, cruise, rda.file = "smb_dashboard.rda") {

  dir.create("data2", showWarnings = FALSE)

  print("Byrjum a mogum")
  hv_pred <-
    res$skraning %>%
    dplyr::filter(!is.na(magaastand)) %>%
    dplyr::select(synis_id,
                  pred = tegund,
                  nr,
                  oslaegt,
                  slaegt,
                  astand = magaastand)
  hv_prey <-
    res$skraning %>%
    dplyr::filter(maeliadgerd %in% c(20, 21)) %>%
    dplyr::rename(prey = tegund,
                  pred = ranfiskurteg,
                  pnr = nr,
                  nr = kvarnanr) %>%
    dplyr::left_join(res$other.stuff$fisktegundir %>%
                       dplyr::select(prey = tegund, heiti),
                     by = "prey") %>%
    dplyr::select(synis_id,
                  pred,
                  nr,
                  prey = heiti,
                  #heiti,
                  pnr,
                  n = fjoldi,
                  lengd,
                  kyn,
                  thyngd = heildarthyngd)
  hv_pred %>%
    dplyr::left_join(hv_prey, by = c("synis_id", "pred", "nr")) %>%
    dplyr::left_join(res$st %>%
                       dplyr::select(synis_id, leidangur, stod),
                     by = "synis_id") %>%
    dplyr::select(leidangur, stod, pred:thyngd) %>%
    readr::write_rds("data2/pp.rds")

  print("read stations from external file")
  tows.external <-
    readr::read_csv(file=paste0(path.package("smxapp"),"/csv/stations_smh.csv"),
                    show_col_types = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(gid = vid) %>%
    dplyr::mutate(lon1 = -gisland::geo_convert(lon1),
                  lon2 = -gisland::geo_convert(lon2),
                  lat1 =  gisland::geo_convert(lat1),
                  lat2 =  gisland::geo_convert(lat2)) %>%
    dplyr::mutate(lon2 = ifelse(square == 424 & townumber == 3, -24.7583, lon2))



  res2 <- res

  print("Various calculations")
  now.year <-
    dplyr::tibble(x = cruise[[1]]) %>%
    tidyr::separate(x, c("ship", "year"), sep = "-", convert = TRUE) %>%
    dplyr::pull(year)


  min.towlength <- 2             # Minimum "acceptable" towlength
  max.towlength <- 8             # Maximum "acceptable" towlength
  std.towlength <- 4             # Standard tow length is 4 nautical miles

  st <-
    res$st %>%
    dplyr::mutate(index = paste0(index, veidarfaeri))
  # if smh (synaflokkur 35) then drop year 2011
  tmp <- st %>% dplyr::pull(synaflokkur) %>% unique()
  if(tmp == 35) {
    print("Dropping year 1995 and 2011")
    st <- st %>% dplyr::filter(!ar %in% c(1995, 2011))
  }
  nu <- res$nu
  le <- res$le
  kv <- res$kv

  st <-
    st %>%
    dplyr::filter((ar == now.year & leidangur %in% cruise) | ar < now.year)
  index.done <-
    st %>%
    dplyr::filter(leidangur %in% cruise) %>%
    dplyr::pull(index)

  i <- stringr::str_locate(st$leidangur, "-")[,1]
  st <-
    st %>%
    dplyr::mutate(leidstod = paste0(stringr::str_sub(leidangur, 1, i), stod))

  nu <-
    nu %>%
    tidyr::complete(synis_id, tegund) %>%
    tidyr::replace_na(list(fj_maelt = 0, fj_talid = 0, fj_alls = 0))

  nu.this.year <-
    st %>%
    dplyr::filter(ar == now.year,
                  index %in% index.done) %>%
    dplyr::select(synis_id, leidangur, stod) %>%
    dplyr::left_join(nu, by = "synis_id")

  le.this.year <-
    st %>%
    dplyr::filter(ar == now.year,
                  index %in% index.done) %>%
    dplyr::select(synis_id, leidangur, stod, index) %>%
    dplyr::left_join(le, by = "synis_id")
  print("Extractions done")

  le <-
    le %>%
    tidyr::complete(synis_id, tegund) %>%
    tidyr::replace_na(list(lengd = 0, fjoldi = 0)) %>%
    dplyr::left_join(nu, by = c("synis_id", "tegund")) %>%
    dplyr::mutate(r = ifelse(fjoldi == 0, 1, fj_alls/fj_maelt),
           n.rai = ifelse(fjoldi != 0, fjoldi * r, fj_alls)) %>%
    dplyr::left_join(st %>%
                       dplyr::select(synis_id, ar, reitur, tognumer, toglengd), by = "synis_id") %>%
    dplyr::mutate(toglengd = dplyr::if_else(toglengd > max.towlength, max.towlength, toglengd),
           toglengd = dplyr::if_else(toglengd < min.towlength, min.towlength, toglengd),
           n.std = n.rai * std.towlength/toglengd,
           b.std  = ifelse(is.na(n.std), 0, n.rai) * 0.00001 * lengd^3) %>%
    dplyr::select(synis_id, ar, reitur, tognumer, toglengd, tegund:n.rai, n.std, b.std)
  print("Length compilation done")


  # B. STADLAR -----------------------------------------------------------------
  other.stuff <- res$other.stuff
  stadlar.rallstodvar <- other.stuff$stadlar.rallstodvar
  stadlar.tegundir <-    other.stuff$stadlar.tegundir
  stadlar.lw <- other.stuff$stadlar.lw
  fisktegundir <- other.stuff$fisktegundir
  print("Importing of auxillary tables done")

  # IMPORT
  # ----------------------------------------------------------------------------


  # ----------------------------------------------------------------------------
  # DATA MUNGING

  # A. STATIONS DONE - FOR DASHBOARD

  by.tegund.lengd.ar <-
    st %>%
    dplyr::filter(index %in% index.done) %>%
    dplyr::select(synis_id) %>%
    dplyr::left_join(le, by = "synis_id") %>%
    dplyr::group_by(tegund, ar, lengd) %>%
    dplyr::summarise(n.std = sum(n.std, na.rm = TRUE),
                     b.std = sum(b.std, na.rm = TRUE),
                     .groups = "drop")
  x <-
    by.tegund.lengd.ar %>%
    # code added because of bug in TL2-2018
    dplyr::select(tegund, lengd) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(tegund) %>%
    dplyr::summarise(n = dplyr::n(),
                     l.min = min(lengd),
                     l.max = max(lengd))
  res <- list()
  for(i in 1:length(x$tegund)) {
    res[[i]] <- expand.grid(tegund = x$tegund[i],
                            lengd = x$l.min[i]:x$l.max[i],
                            ar = unique(by.tegund.lengd.ar$ar))
  }
  x <- dplyr::bind_rows(res) %>% tibble::as_tibble()

  by.tegund.lengd.ar <-
    x %>%
    dplyr::left_join(by.tegund.lengd.ar, by = c("tegund", "lengd", "ar")) %>%
    dplyr::mutate(n.std = ifelse(is.na(n.std), 0, n.std),
                  b.std = ifelse(is.na(b.std), 0, b.std))

  by.tegund.lengd.ar.m <-
    by.tegund.lengd.ar %>%
    dplyr::filter(ar >= 2010) %>%
    dplyr::group_by(tegund, lengd) %>%
    dplyr::summarise(n.year = dplyr::n_distinct(ar),
              n.std = sum(n.std, na.rm = TRUE) / n.year,
              b.std = sum(b.std, na.rm = TRUE) / n.year,
              .groups = "drop")
  print("Length summation 1 done")

  by.station <-
    st %>%
    dplyr::filter(index %in% index.done) %>%
    dplyr::select(synis_id, lon, lat, index) %>%
    dplyr::left_join(le, by = "synis_id") %>%
    dplyr::group_by(ar, index, lon, lat, tegund) %>%
    dplyr::summarise(n.std = sum(n.std, na.rm = TRUE),
              b.std = sum(b.std, na.rm = TRUE),
              .groups = "drop")
  print("Station summation done")

  kv.this.year <-
    st %>%
    dplyr::filter(ar == now.year,
           index %in% index.done) %>%
    dplyr::select(synis_id, index, leidstod) %>%
    dplyr::left_join(kv, by = "synis_id") %>%
    dplyr:: mutate(lab = paste0(leidstod, "-", nr)) %>%
    dplyr::left_join(stadlar.lw, by = c("tegund", "lengd")) %>%
    dplyr::mutate(ok.l.osl = dplyr::if_else(oslaegt >= osl1 & oslaegt <= osl2, TRUE, FALSE, TRUE),
           ok.l.sl = dplyr::if_else(slaegt >= sl1 & slaegt <= sl2, TRUE, FALSE, TRUE)) %>%
    dplyr::select(-c(osl1:sl2)) %>%
    dplyr::left_join(stadlar.tegundir %>%
                dplyr::select(tegund, kynkirtlar_high:oslaegt_vigtad_low), by = "tegund") %>%
    dplyr::mutate(ok.sl.osl = dplyr::if_else(slaegt/oslaegt >= oslaegt_slaegt_low & slaegt/oslaegt <= oslaegt_slaegt_high, TRUE, FALSE, TRUE),
           ok.kirtlar.osl = dplyr::if_else(kynfaeri/oslaegt >= kynkirtlar_low & kynfaeri/oslaegt <= kynkirtlar_high, TRUE, FALSE, TRUE),
           ok.lifur.osl = dplyr::if_else(lifur/oslaegt >= lifur_low & lifur/oslaegt <= lifur_high, TRUE, FALSE, TRUE),
           ok.magir.osl = dplyr::if_else(magi/oslaegt  >= magi_low & magi/oslaegt <= magi_high, TRUE, FALSE, TRUE)) %>%
    dplyr::select(-c(kynkirtlar_high:oslaegt_vigtad_low))
  print("Otolith summation done")

  le.this.year <-
    le.this.year %>%
    dplyr::left_join(stadlar.tegundir %>%
                       dplyr::select(tegund, lengd_low, lengd_high),
                     by = "tegund") %>%
    dplyr::mutate(ok.l = dplyr::if_else(lengd >= lengd_low & lengd <= lengd_high, TRUE, FALSE, TRUE)) %>%
    dplyr::select(-c(lengd_low, lengd_high))
  print("Length summation 2 done")

  my_boot = function(x, times=100) {

    # Get column name from input object
    var = deparse(substitute(x))
    var = gsub("^\\.\\$","", var)

    # Bootstrap 95% CI
    cis = stats::quantile(replicate(times, mean(sample(x, replace=TRUE))), probs=c(0.025,0.975))

    # Return data frame of results
    data.frame(var, n=length(x), mean=mean(x), lower.ci=cis[1], upper.ci=cis[2])
  }

  print("Bootstrapping abundance:")

  by.station.boot.n <-
    by.station %>%
    dplyr::group_by(tegund, ar) %>%
    dplyr::do(my_boot(.$n.std)) %>%
    dplyr::mutate(variable = "n",
                  var = as.character(var))

  print("Bootstrapping biomass:")

  by.station.boot.b <-
    by.station %>%
    dplyr::group_by(tegund, ar) %>%
    dplyr::do(my_boot(.$b.std)) %>%
    dplyr::mutate(variable = "b",
                  var = as.character(var))

  by.station.boot <-
    dplyr::bind_rows(by.station.boot.n, by.station.boot.b)

  #-----------------------------------------------------------------------
  # New stuff: boot station and then calc uncertainty by length


  # Do for all stations -------------------------------------------------
  by.station.all <-
    st %>%
    dplyr::select(synis_id, lon, lat, index) %>%
    dplyr::left_join(le, by = "synis_id") %>%
    dplyr::group_by(ar, index, lon, lat, tegund) %>%
    dplyr::summarise(n.std = sum(n.std, na.rm = TRUE),
              b.std = sum(b.std, na.rm = TRUE),
              groups = "drop")


  print("Spatial stuff - the new kid on the block (sf)")

  stadlar.rallstodvar.sf <-
    tows.external %>%
    dplyr::select(square:townumber, gid,
                  lon_start = lon1, lat_start = lat1,
                  lon_end   = lon2, lat_end   = lat2) %>%
    tidyr::pivot_longer(lon_start:lat_end,
                        names_to = c(".value", "startend"),
                        names_sep = "_") %>%
    sf::st_as_sf(coords = c("lon", "lat"),
                 crs = 4326) %>%
    dplyr::group_by(square, townumber, gid) %>%
    dplyr::summarise(do_union = FALSE) %>%
    sf::st_cast("LINESTRING") %>%
    dplyr::mutate(index = paste0(square,
                          ifelse(townumber < 10,
                                 paste0("0", townumber),
                                 townumber),
                          gid),
                  .groups = "drop")


  # SF DONE
  st.done.sf <-
    st %>%
    dplyr::filter(index %in% index.done) %>%
    dplyr::filter(!is.na(lon1), !is.na(lat1), !is.na(lon2), !is.na(lat2)) %>%
    dplyr::mutate(year = lubridate::year(dags)) %>%
    dplyr::select(index,
                  gid = veidarfaeri,
                  year,
                  lon_start = lon1, lat_start = lat1,
                  lon_end = lon2, lat_end = lat2) %>%
    tidyr::pivot_longer(lon_start:lat_end,
                 names_to = c(".value", "startend"),
                 names_sep = "_") %>%
    #dplyr::select(-startend) %>%
    sf::st_as_sf(coords = c("lon", "lat"),
             crs = 4326) %>%
    dplyr::group_by(index, year, gid) %>%
    dplyr::summarise(do_union = FALSE) %>%
    sf::st_cast("LINESTRING") %>%
    dplyr::ungroup()

  # END: SF DONE

  print("Spatial stuff - the old faithful")

  library(sp)
  tows <-
    stadlar.rallstodvar %>%
    tidyr::drop_na(kastad_v, kastad_n, hift_v, hift_n)

  tows$id2 <- 1:nrow(tows)
  x1 <-
    tows %>%
    dplyr::select(id2, kastad_v, hift_v) %>%
    dplyr::gather(variable, value, -id2)
  x2 <-
    tows %>%
    dplyr::select(id2, kastad_n, hift_n) %>%
    dplyr::gather(variable, value, -id2)
  x <- data.frame(id2 = x1$id2, lon = x1$value, lat = x2$value)
  lines_list <- list()
  for (i in 1:max(tows$id2)) {
    x2 <- sp::Line(x[x$id2 == i,c("lon","lat")])
    lines_list[[i]] <- sp::Lines(list(x2),ID=as.character(tows$id2[i]))
  }
  sp <-
    lines_list %>%
    sp::SpatialLines(proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>%
    sp::SpatialLinesDataFrame(data.frame(id = as.character(tows$id2)))
  sp@data <- cbind(sp@data, tows)
  stadlar.rallstodvar.sp <- sp

  tows <-
    st %>%
    dplyr::filter(index %in% index.done) %>%
    dplyr::filter(!is.na(lon1), !is.na(lat1), !is.na(lon2), !is.na(lat2))
  tows$id2 <- 1:nrow(tows)
  x1 <-
    tows %>%
    dplyr::select(id2, lon1, lon2) %>%
    tidyr::gather(variable, value, -id2)
  x2 <-
    tows %>%
    dplyr::select(id2, lat1, lat2) %>%
    tidyr::gather(variable, value, -id2)
  x <- data.frame(id2 = x1$id2, lon = x1$value, lat = x2$value)
  lines_list <- list()
  for (i in 1:max(tows$id2)) {
    x2 <- sp::Line(x[x$id2 == i,c("lon","lat")])
    lines_list[[i]] <- sp::Lines(list(x2),ID=as.character(tows$id2[i]))
  }
  sp <-
    lines_list %>%
    sp::SpatialLines(proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>%
    sp::SpatialLinesDataFrame(data.frame(id = as.character(tows$id2)))
  sp@data <- cbind(sp@data, tows)
  st.done.sp <- sp



  by.tegund.lengd.ar <-
    by.tegund.lengd.ar %>%
    dplyr::filter(lengd != 0)
  by.tegund.lengd.ar.m <-
    by.tegund.lengd.ar.m %>%
    dplyr::filter(lengd != 0)

  timi <- lubridate::now() %>% as.character()

  print("Saving")

  dir.create("data2", showWarnings = FALSE)
  save(now.year,
       index.done,
       stadlar.rallstodvar.sp,
       stadlar.rallstodvar.sf,
       st.done.sp,
       st.done.sf,
       st,
       stadlar.tegundir,
       stadlar.lw,
       timi,
       kv.this.year,
       le.this.year,
       nu.this.year,
       by.tegund.lengd.ar, by.tegund.lengd.ar.m,
       by.station, fisktegundir,
       by.station.boot,
       #by.station.boot.all,
       file = paste0("data2/", rda.file))

  print(paste0("Data saved as data2/", rda.file))

  readr::write_rds(res2, "data2/res.rds")



  print("HURRA")


}
