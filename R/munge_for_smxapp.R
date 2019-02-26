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
                       select(prey = tegund, heiti)) %>%
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
    dplyr::left_join(hv_prey) %>%
    dplyr::left_join(res$st %>%
                       select(synis_id, leidangur, stod)) %>%
    dplyr::select(leidangur, stod, pred:thyngd) %>%
    readr::write_rds("data2/pp.rds")

  res2 <- res

  print("Various calculations")
  now.year <-
    dplyr::tibble(x = cruise[[1]]) %>%
    tidyr::separate(x, c("ship", "year"), sep = "-", convert = TRUE) %>%
    dplyr::pull(year)


  min.towlength <- 2             # Minimum "acceptable" towlength
  max.towlength <- 8             # Maximum "acceptable" towlength
  std.towlength <- 4             # Standard tow length is 4 nautical miles

  st <- res$st
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
    complete(synis_id, tegund) %>%
    replace_na(list(lengd = 0, fjoldi = 0)) %>%
    left_join(nu, by = c("synis_id", "tegund")) %>%
    mutate(r = ifelse(fjoldi == 0, 1, fj_alls/fj_maelt),
           n.rai = ifelse(fjoldi != 0, fjoldi * r, fj_alls)) %>%
    left_join(st %>% select(synis_id, ar, reitur, tognumer, toglengd), by = "synis_id") %>%
    mutate(toglengd = if_else(toglengd > max.towlength, max.towlength, toglengd),
           toglengd = if_else(toglengd < min.towlength, min.towlength, toglengd),
           n.std = n.rai * std.towlength/toglengd,
           b.std  = ifelse(is.na(n.std), 0, n.rai) * 0.00001 * lengd^3) %>%
    select(synis_id, ar, reitur, tognumer, toglengd, tegund:n.rai, n.std, b.std)
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
                     b.std = sum(b.std, na.rm = TRUE)) %>%
    dplyr::ungroup()
  x <-
    by.tegund.lengd.ar %>%
    # code added because of bug in TL2-2018
    dplyr::select(tegund, lengd) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(tegund) %>%
    dplyr::summarise(n = n(),
                     l.min = min(lengd),
                     l.max = max(lengd))
  res <- list()
  for(i in 1:length(x$tegund)) {
    res[[i]] <- expand.grid(tegund = x$tegund[i],
                            lengd = x$l.min[i]:x$l.max[i],
                            ar = unique(by.tegund.lengd.ar$ar))
  }
  x <- dplyr::bind_rows(res) %>% as_tibble()

  by.tegund.lengd.ar <-
    x %>%
    dplyr::left_join(by.tegund.lengd.ar, by = c("tegund", "lengd", "ar")) %>%
    dplyr::mutate(n.std = ifelse(is.na(n.std), 0, n.std),
                  b.std = ifelse(is.na(b.std), 0, b.std))

  by.tegund.lengd.ar.m <-
    by.tegund.lengd.ar %>%
    dplyr::filter(ar >= 2010) %>%
    group_by(tegund, lengd) %>%
    summarise(n.year = n_distinct(ar),
              n.std = sum(n.std, na.rm = TRUE) / n.year,
              b.std = sum(b.std, na.rm = TRUE) / n.year) %>%
    ungroup()
  print("Length summation 1 done")

  by.station <-
    st %>%
    filter(index %in% index.done) %>%
    select(synis_id, lon, lat, index) %>%
    left_join(le, by = "synis_id") %>%
    group_by(ar, index, lon, lat, tegund) %>%
    summarise(n.std = sum(n.std, na.rm = TRUE),
              b.std = sum(b.std, na.rm = TRUE)) %>%
    ungroup()
  print("Station summation done")

  kv.this.year <-
    st %>%
    filter(ar == now.year,
           index %in% index.done) %>%
    select(synis_id, index, leidstod) %>%
    left_join(kv, by = "synis_id") %>%
    mutate(lab = paste0(leidstod, "-", nr)) %>%
    left_join(stadlar.lw, by = c("tegund", "lengd")) %>%
    mutate(ok.l.osl = if_else(oslaegt >= osl1 & oslaegt <= osl2, TRUE, FALSE, TRUE),
           ok.l.sl = if_else(slaegt >= sl1 & slaegt <= sl2, TRUE, FALSE, TRUE)) %>%
    select(-c(osl1:sl2)) %>%
    left_join(stadlar.tegundir %>%
                select(tegund, kynkirtlar_high:oslaegt_vigtad_low), by = "tegund") %>%
    mutate(ok.sl.osl = if_else(slaegt/oslaegt >= oslaegt_slaegt_low & slaegt/oslaegt <= oslaegt_slaegt_high, TRUE, FALSE, TRUE),
           ok.kirtlar.osl = if_else(kynfaeri/oslaegt >= kynkirtlar_low & kynfaeri/oslaegt <= kynkirtlar_high, TRUE, FALSE, TRUE),
           ok.lifur.osl = if_else(lifur/oslaegt >= lifur_low & lifur/oslaegt <= lifur_high, TRUE, FALSE, TRUE),
           ok.magir.osl = if_else(magi/oslaegt  >= magi_low & magi/oslaegt <= magi_high, TRUE, FALSE, TRUE)) %>%
    select(-c(kynkirtlar_high:oslaegt_vigtad_low))
  print("Otolith summation done")

  le.this.year <-
    le.this.year %>%
    dplyr::left_join(stadlar.tegundir %>%
                       select(tegund, lengd_low, lengd_high),
                     by = "tegund") %>%
    dplyr::mutate(ok.l = if_else(lengd >= lengd_low & lengd <= lengd_high, TRUE, FALSE, TRUE)) %>%
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
    do(my_boot(.$n.std)) %>%
    dplyr::mutate(variable = "n",
                  var = as.character(var))

  print("Bootstrapping biomass:")

  by.station.boot.b <-
    by.station %>%
    dplyr::group_by(tegund, ar) %>%
    do(my_boot(.$b.std)) %>%
    dplyr::mutate(variable = "b",
                  var = as.character(var))

  by.station.boot <-
    dplyr::bind_rows(by.station.boot.n, by.station.boot.b)

  #-----------------------------------------------------------------------
  # New stuff: boot station and then calc uncertainty by length


  # Do for all stations -------------------------------------------------
  by.station.all <-
    st %>%
    select(synis_id, lon, lat, index) %>%
    left_join(le, by = "synis_id") %>%
    group_by(ar, index, lon, lat, tegund) %>%
    summarise(n.std = sum(n.std, na.rm = TRUE),
              b.std = sum(b.std, na.rm = TRUE)) %>%
    ungroup()




  print("Spatial stuff")

  library(sp)
  tows <-
    stadlar.rallstodvar %>%
    drop_na(kastad_v, kastad_n, hift_v, hift_n)

  tows$id2 <- 1:nrow(tows)
  x1 <-
    tows %>%
    select(id2, kastad_v, hift_v) %>%
    gather(variable, value, -id2)
  x2 <-
    tows %>%
    select(id2, kastad_n, hift_n) %>%
    gather(variable, value, -id2)
  x <- data.frame(id2 = x1$id2, lon = x1$value, lat = x2$value)
  lines_list <- list()
  for (i in 1:max(tows$id2)) {
    x2 <- Line(x[x$id2 == i,c("lon","lat")])
    lines_list[[i]] <- Lines(list(x2),ID=as.character(tows$id2[i]))
  }
  sp <-
    lines_list %>%
    SpatialLines(proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>%
    SpatialLinesDataFrame(data.frame(id = as.character(tows$id2)))
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
    x2 <- Line(x[x$id2 == i,c("lon","lat")])
    lines_list[[i]] <- Lines(list(x2),ID=as.character(tows$id2[i]))
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
       st.done.sp,
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
