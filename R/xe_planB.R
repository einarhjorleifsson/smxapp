lesa_stodvarB <- function(con, schema = "FISKAR", synaflokkur) {
  # stodvar
  q <-
    paste0('select * from ', schema, '.STODVAR WHERE synaflokkur = ', synaflokkur)
  st <-
    DBI::dbGetQuery(con, q) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::select(synis_id:heildarafli,
                  synaflokkur) %>%
    dplyr::mutate(ar = year(dags))
  # togstodvar
  q <-
    paste0('select * from ', schema, '.TOGSTODVAR')
  to <-
    DBI::dbGetQuery(con, q) %>%
    tibble::as_tibble() %>%
    janitor::clean_names()  %>%
    dplyr::right_join(st %>% dplyr::select(synis_id)) %>%
    dplyr::select(synis_id:eykt)
  # umhverfi
  q <-
    paste0('select * from ', schema, '.UMHVERFI')
  um <-
    DBI::dbGetQuery(con, q) %>%
    tibble::as_tibble() %>%
    janitor::clean_names()  %>%
    dplyr::right_join(st %>% select(synis_id)) %>%
    dplyr::select(synis_id:sjondypi)
  st %>%
    dplyr::left_join(to) %>%
    dplyr::left_join(um)

}
#' @export
lesa_numerB <- function(con, schema = "FISKAR") {
  # numer
  q <-
    paste0('select * from ', schema, '.NUMER')
  DBI::dbGetQuery(con, q) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::select(synis_id,
                  tegund, fj_maelt, fj_talid, fj_kyngreint,
                  fj_vigtad, fj_magasyna) %>%
    dplyr::mutate(fj_maelt =     replace_na(fj_maelt, 0),
                  fj_talid =     replace_na(fj_talid, 0),
                  fj_kyngreint = replace_na(fj_kyngreint, 0),
                  fj_vigtad =    replace_na(fj_vigtad, 0),
                  fj_magasyna =  replace_na(fj_magasyna, 0),
                  fj_alls = fj_maelt + fj_talid)
}
#' @export
lesa_lengdirB <- function(con, schema = "FISKAR") {
  q <-
    paste0('select * from ', schema, '.LENGDIR')
  DBI::dbGetQuery(con, q) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::select(synis_id:kynthroski)

}
#' @export
lesa_kvarnirB <- function(con, schema = "FISKAR") {
  q <-
    paste0('select * from ', schema, '.KVARNIR')
  DBI::dbGetQuery(con, q) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::select(synis_id:magi)
}
#' @export
hv_skraningB <- function(con) {
  DBI::dbGetQuery(con, "select * from HAFVOG.SKRANING") %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::select(-c(snt:sbn))
}
#' @export
compileB <- function(con, schema = "FISKAR", synaflokkur = 35) {
  res <- list()
  res$st <-
    lesa_stodvarB(con, schema, synaflokkur = synaflokkur) %>%
    dplyr::mutate(index = reitur * 100 + tognumer) %>%
    dplyr::mutate(lon1 = -kastad_v_lengd,
                  lat1 = kastad_n_breidd,
                  lon2 = -hift_v_lengd,
                  lat2 = hift_n_breidd) %>%
    geo::geoconvert(col.names = c("lat1", "lon1")) %>%
    geo::geoconvert(col.names = c("lat2", "lon2")) %>%
    dplyr::mutate(lon = (lon1 + lon2) / 2,
                  lat = (lat1 + lat2) / 2,
                  toglengd = ifelse(is.na(toglengd), 4, toglengd))
  res$nu <-
    lesa_numerB(con, schema) %>%
    dplyr::right_join(res$st %>% dplyr::select(synis_id))
  res$le <-
    lesa_lengdirB(con, schema) %>%
    dplyr::right_join(res$st %>% dplyr::select(synis_id))
  res$ot <-
    lesa_kvarnirB(con, schema) %>%
    dplyr::right_join(res$st %>% dplyr::select(synis_id))

  return(res)
}

# ----------------------------------------------------------------------------
# Other stuff needed from hafvog
# B. STADLAR -----------------------------------------------------------------
#' @export
hv_stadla_rallstodvarB <- function(con) {
  q <-
    d1 <-
    DBI::dbGetQuery(con, 'select * from HAFVOG.STI_RALLSTODVAR') %>%
    tibble::as_tibble() %>%
    janitor::clean_names()
  d2 <-
    DBI::dbGetQuery(con, 'select * from HAFVOG.STI_LEIDANGRAR') %>%
    tibble::as_tibble() %>%
    janitor::clean_names()
  d1 %>%
    dplyr::left_join(d2) %>%
    dplyr::mutate(kastad_v = -kastad_v, hift_v = -hift_v,
                  index = reitur * 100 + tognumer)
}
#' @export
hv_stadlar_tegundirB <- function(con) {
  DBI::dbGetQuery(con, "select * from HAFVOG.FISKTEG_TEGUNDIR") %>%
    tibble::as_tibble() %>%
    janitor::clean_names()
}
#' @export
hv_lwB <- function(con) {
  DBI::dbGetQuery(con, "select * from HAFVOG.FISKTEG_LENGD_THYNGD") %>%
    tibble::as_tibble() %>%
    janitor::clean_names()
}
#' @export
hv_fisktegundirB <- function(con) {
  DBI::dbGetQuery(con, "select * from HAFVOG.FISKTEGUNDIR") %>%
    tibble::as_tibble() %>%
    janitor::clean_names()
}
#' @export
hv_faeduhoparB <- function(con) {
  DBI::dbGetQuery(con, "select * from HAFVOG.F_TEGUNDIR") %>%
    tibble::as_tibble() %>%
    janitor::clean_names()
}
#' @export
hv_maeliatridiB <- function(con) {
  DBI::dbGetQuery(con, "select * from HAFVOG.MAELIATRIDI") %>%
    tibble::as_tibble() %>%
    janitor::clean_names() #%>%
  #dplyr::rename(aid = id, adgerd = heiti)
}
