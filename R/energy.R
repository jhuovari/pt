# Täydentävä fyysinen aineisto: teollisuuden energiankäyttö (StatFin tene/11wy).

#' Teollisuuden energiankäyttö toimialaryhmittäin ja energialähteittäin
#'
#' Hakee Tilastokeskuksen teollisuuden energiankäyttötilaston (taulukko
#' `tene/11wy.px`). Aineisto täydentää panos-tuotostaulukoiden rahamääräisiä
#' tietoja: siitä nähdään sähkön ja öljyn määrä (GWh) ja osuus energiankäytöstä.
#' Huom. sähkön käyttöön sisältyy myös teollisuuden itse tuottama sähkö.
#'
#' @param years Vuodet (oletus: kaikki).
#' @param unit `"ek_gwh"` (GWh) tai `"ek_tj"` (TJ).
#' @param cache Käytetäänkö välimuistia.
#' @return data.frame: `year`, `group`, `group_label`, `source`,
#'   `source_label`, `value`.
#' @export
industry_energy_use <- function(years = NULL, unit = c("ek_gwh", "ek_tj"), cache = TRUE) {
  unit <- match.arg(unit)
  sel <- list(contentscode = unit)
  if (!is.null(years)) sel$timeperiod_y <- as.character(years)
  d <- statfin_get("tene/11wy.px", sel, cache = cache)
  data.frame(year = as.integer(d$timeperiod_y),
             group = d$toimiala_500_20110101,
             group_label = d$toimiala_500_20110101_label,
             source = d$polttoaineet_50_20110101,
             source_label = d$polttoaineet_50_20110101_label,
             value = d$value, stringsAsFactors = FALSE)
}

#' Panos-tuotostoimialojen vastaavuus energiatilaston toimialaryhmiin
#'
#' @return data.frame: `industry` (panos-tuotostaulukon koodi) ja `group`
#'   (taulukon `tene/11wy` toimialaluokka).
#' @export
energy_group_map <- function() {
  m <- list(
    "01" = "B", "02" = "C10TC12", "03" = "C13TC15", "04" = c("C16", "C17"),
    "05" = c("C19", "C20", "C21", "C22"), "06" = "C24",
    "07" = c("C25", "C28", "C29", "C30", "C33"), "08" = c("C26", "C27"),
    "09" = c("C18", "C23", "C31_C32")
  )
  data.frame(industry = unlist(m, use.names = FALSE),
             group = rep(names(m), lengths(m)), stringsAsFactors = FALSE)
}
