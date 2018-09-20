#' Create R-binary bundle for the smxapp
#'
#' @param res a list contain st, nu, le and kv and other things. The object is
#' generated via function import_smx (in the xe-package)
#' @param cruise leidangrar in schema hafvog
#' @param rda.file name the exported binary file (default smb_dashboard.rda)
#' which is stored in the directory data2
#'
#' @export
#'
# cruise = c("A4-2018", "TL1-2018", "TH1-2018", "B3-2018")
munge_for_smxapp <- function(res, cruise, rda.file = "smb_dashboard.rda") {

  now.year <-
    data_frame(x = cruise[[1]]) %>%
    separate(x, c("ship", "year"), sep = "-", convert = TRUE) %>%
    pull(year)


  min.towlength <- 2             # Minimum "acceptable" towlength
  max.towlength <- 8             # Maximum "acceptable" towlength
  std.towlength <- 4             # Standard tow length is 4 nautical miles

  st <- res$st
  nu <- res$nu
  le <- res$le
  kv <- res$kv

  st <-
    st %>%
    filter((ar == now.year & leidangur %in% cruise) | ar < now.year)
  index.done <-
    st %>%
    filter(leidangur %in% cruise) %>%
    pull(index)

  i <- stringr::str_locate(st$leidangur, "-")[,1]
  st <-
    st %>%
    mutate(leidstod = paste0(stringr::str_sub(leidangur, 1, i), stod))

  nu <-
    nu %>%
    complete(synis_id, tegund) %>%
    replace_na(list(fj_maelt = 0, fj_talid = 0, fj_alls = 0))

  nu.this.year <-
    st %>%
    filter(ar == now.year,
           index %in% index.done) %>%
    select(synis_id, leidangur, stod) %>%
    left_join(nu, by = "synis_id")

  le.this.year <-
    st %>%
    filter(ar == now.year,
           index %in% index.done) %>%
    select(synis_id, leidangur, stod, index) %>%
    left_join(le, by = "synis_id")

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


  # B. STADLAR -----------------------------------------------------------------
  other.stuff <- res$other.stuff
  stadlar.rallstodvar <- other.stuff$stadlar.rallstodvar
  stadlar.tegundir <-    other.stuff$stadlar.tegundir
  stadlar.lw <- other.stuff$stadlar.lw
  fisktegundir <- other.stuff$fisktegundir

  # IMPORT
  # ----------------------------------------------------------------------------


  # ----------------------------------------------------------------------------
  # DATA MUNGING

  # A. STATIONS DONE - FOR DASHBOARD
  print("Reikna allskonar dot")


  by.tegund.lengd.ar <-
    st %>%
    filter(index %in% index.done) %>%
    select(synis_id) %>%
    left_join(le, by = "synis_id") %>%
    group_by(tegund, ar, lengd) %>%
    summarise(n.std = sum(n.std, na.rm = TRUE),
              b.std = sum(b.std, na.rm = TRUE)) %>%
    ungroup()
  x <-
    by.tegund.lengd.ar %>%
    group_by(tegund) %>%
    summarise(n = n(),
              l.min = min(lengd),
              l.max = max(lengd))
  res <- list()
  for(i in 1:length(x$tegund)) {
    res[[i]] <- expand.grid(tegund = x$tegund[i],
                            lengd = x$l.min[i]:x$l.max[i],
                            ar = unique(by.tegund.lengd.ar$ar))
  }
  x <- bind_rows(res) %>% as_tibble()

  by.tegund.lengd.ar <-
    x %>%
    left_join(by.tegund.lengd.ar, by = c("tegund", "lengd", "ar")) %>%
    mutate(n.std = ifelse(is.na(n.std), 0, n.std),
           b.std = ifelse(is.na(b.std), 0, b.std))

  by.tegund.lengd.ar.m <-
    by.tegund.lengd.ar %>%
    filter(ar >= 2010) %>%
    group_by(tegund, lengd) %>%
    summarise(n.year = n_distinct(ar),
              n.std = sum(n.std, na.rm = TRUE) / n.year,
              b.std = sum(b.std, na.rm = TRUE) / n.year) %>%
    ungroup()

  by.station <-
    st %>%
    filter(index %in% index.done) %>%
    select(synis_id, lon, lat, index) %>%
    left_join(le, by = "synis_id") %>%
    group_by(ar, index, lon, lat, tegund) %>%
    summarise(n.std = sum(n.std, na.rm = TRUE),
              b.std = sum(b.std, na.rm = TRUE)) %>%
    ungroup()

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

  le.this.year <-
    le.this.year %>%
    left_join(stadlar.tegundir %>% select(tegund, lengd_low, lengd_high)) %>%
    mutate(ok.l = if_else(lengd >= lengd_low & lengd <= lengd_high, TRUE, FALSE, TRUE)) %>%
    select(-c(lengd_low, lengd_high))

  my_boot = function(x, times=100) {

    # Get column name from input object
    var = deparse(substitute(x))
    var = gsub("^\\.\\$","", var)

    # Bootstrap 95% CI
    cis = quantile(replicate(times, mean(sample(x, replace=TRUE))), probs=c(0.025,0.975))

    # Return data frame of results
    data.frame(var, n=length(x), mean=mean(x), lower.ci=cis[1], upper.ci=cis[2])
  }

  print("Hartoga fjolda")

  by.station.boot.n <-
    by.station %>%
    group_by(tegund, ar) %>%
    do(my_boot(.$n.std)) %>%
    mutate(variable = "n",
           var = as.character(var))

  print("Hartoga thyngd")

  by.station.boot.b <-
    by.station %>%
    group_by(tegund, ar) %>%
    do(my_boot(.$b.std)) %>%
    mutate(variable = "b",
           var = as.character(var))

  by.station.boot <-
    bind_rows(by.station.boot.n, by.station.boot.b)

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

  # print("Hartoga fjolda - all")
  #
  # by.station.boot.n.all <-
  #   by.station.all %>%
  #   group_by(tegund, ar) %>%
  #   do(my_boot(.$n.std)) %>%
  #   mutate(variable = "n",
  #          var = as.character(var))
  #
  # print("Hartoga thyngd - all")
  #
  # by.station.boot.b.all <-
  #   by.station.all %>%
  #   group_by(tegund, ar) %>%
  #   do(my_boot(.$b.std)) %>%
  #   mutate(variable = "b",
  #          var = as.character(var))
  #
  # by.station.boot.all <-
  #   bind_rows(by.station.boot.n.all, by.station.boot.b.all)


  print("Vidoma dot")

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
    SpatialLines(proj4string = PRO) %>%
    SpatialLinesDataFrame(data.frame(id = as.character(tows$id2)))
  sp@data <- cbind(sp@data, tows)
  stadlar.rallstodvar.sp <- sp

  tows <-
    st %>%
    filter(index %in% index.done) %>%
    filter(!is.na(lon1), !is.na(lat1), !is.na(lon2), !is.na(lat2))
  tows$id2 <- 1:nrow(tows)
  x1 <-
    tows %>%
    select(id2, lon1, lon2) %>%
    gather(variable, value, -id2)
  x2 <-
    tows %>%
    select(id2, lat1, lat2) %>%
    gather(variable, value, -id2)
  x <- data.frame(id2 = x1$id2, lon = x1$value, lat = x2$value)
  lines_list <- list()
  for (i in 1:max(tows$id2)) {
    x2 <- Line(x[x$id2 == i,c("lon","lat")])
    lines_list[[i]] <- Lines(list(x2),ID=as.character(tows$id2[i]))
  }
  sp <-
    lines_list %>%
    SpatialLines(proj4string = PRO) %>%
    SpatialLinesDataFrame(data.frame(id = as.character(tows$id2)))
  sp@data <- cbind(sp@data, tows)
  st.done.sp <- sp

  by.tegund.lengd.ar <-
    by.tegund.lengd.ar %>%
    filter(lengd != 0)
  by.tegund.lengd.ar.m <-
    by.tegund.lengd.ar.m %>%
    filter(lengd != 0)

  timi <- lubridate::now() %>% as.character()

  print("Vistun")

  dir.create("data2", showWarnings = FALSE)
  save(index.done,
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

  print("Ormurinn hefur lokid ser af")


}
