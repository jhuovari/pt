# Panos-tuotostaulukoiden haku (StatFin, aihealue "pt").

pt_table_info <- list(
  iot       = list(table = "pt/14yn.px",
                   row = "yhdistelma_1_20180101-tol_tt_lisa_rivi",
                   col = "yhdistelma_1_20180101-tol_tt_lisa_sarake"),
  use_basic = list(table = "pt/14ym.px", row = "yhdistelma_2_20180101",
                   col = "yhdistelma_1_20180101"),
  use_purchaser = list(table = "pt/14yg.px", row = "yhdistelma_2_20180101",
                       col = "yhdistelma_1_20180101"),
  imports   = list(table = "pt/14yp.px", row = "yhdistelma_2_20180101",
                   col = "yhdistelma_1_20180101")
)

#' Luettelo StatFinin panos-tuotostaulukoista
#'
#' @return data.frame: taulukon tunnus, nimi ja päivitysaika.
#' @export
pt_tables <- function() {
  m <- statfin_meta("pt/", cache = FALSE)
  data.frame(id = vapply(m, function(x) x$id, ""),
             text = vapply(m, function(x) x$text, ""),
             updated = vapply(m, function(x) x$updated, ""),
             stringsAsFactors = FALSE)
}

#' Saatavilla olevat panos-tuotosvuodet
#'
#' @return Kokonaislukuvektori vuosista, joilta symmetrinen
#'   panos-tuotostaulukko (14yn) on saatavilla.
#' @export
pt_years <- function() {
  v <- statfin_variables("pt/14yn.px")
  as.integer(v$code[v$variable == "timeperiod_y"])
}

#' Hae panos-tuotostaulukko pitkässä muodossa
#'
#' Yhtenäinen rajapinta StatFinin panos-tuotostaulukoihin. Rivit ovat
#' tuotteita tai toimialoja sekä arvonlisäyksen erät, sarakkeet käyttäviä
#' toimialoja ja loppukäyttöeriä. Arvot ovat miljoonaa euroa käypiin hintoihin
#' (työlliset 1000 henkeä).
#'
#' @param type Taulukko:
#'   * `"iot"` – symmetrinen toimiala x toimiala -panos-tuotostaulukko
#'     perushintaan, kotimaiset tuotteet (14yn),
#'   * `"use_basic"` – käyttötaulukko perushintaan, tuote x toimiala,
#'     kotimaiset + tuonti (14ym),
#'   * `"use_purchaser"` – käyttötaulukko ostajanhintaan, sisältää tuoteverot
#'     ja kauppa- ja kuljetusmarginaalit tuotteiden hinnoissa (14yg),
#'   * `"imports"` – tuonnin käyttötaulukko perushintaan (14yp).
#' @param years Vuodet. Oletuksena kaikki saatavilla olevat.
#' @param cache Käytetäänkö välimuistia.
#' @return data.frame: `year`, `row`, `row_label`, `col`, `col_label`, `value`.
#' @export
pt_get <- function(type = c("iot", "use_basic", "use_purchaser", "imports"),
                   years = NULL, cache = TRUE) {
  type <- match.arg(type)
  info <- pt_table_info[[type]]
  sel <- list(contentscode = "pt-cp")
  if (!is.null(years)) sel$timeperiod_y <- as.character(years)
  df <- statfin_get(info$table, sel, cache = cache)
  out <- data.frame(
    year = as.integer(df$timeperiod_y),
    row = df[[info$row]], row_label = df[[paste0(info$row, "_label")]],
    col = df[[info$col]], col_label = df[[paste0(info$col, "_label")]],
    value = df$value,
    stringsAsFactors = FALSE
  )
  out$value[is.na(out$value)] <- 0
  attr(out, "table") <- info$table
  out
}

#' Muunna pitkä taulukko matriisiksi
#'
#' @param df [pt_get()]:n palauttama data.frame (yksi vuosi).
#' @param rows,cols Valittavat rivi- ja sarakekoodit (oletus: kaikki,
#'   taulukon järjestyksessä).
#' @return Numeerinen matriisi, rivi- ja sarakenimet koodeina.
#' @export
pt_matrix <- function(df, rows = NULL, cols = NULL) {
  if (length(unique(df$year)) > 1) stop("Valitse yksi vuosi ennen pt_matrix()-kutsua.")
  rows <- if (is.null(rows)) unique(df$row) else rows
  cols <- if (is.null(cols)) unique(df$col) else cols
  m <- matrix(0, length(rows), length(cols), dimnames = list(rows, cols))
  d <- df[df$row %in% rows & df$col %in% cols, ]
  m[cbind(match(d$row, rows), match(d$col, cols))] <- d$value
  m
}

pt_labels <- function(df) {
  lab <- c(stats::setNames(df$row_label, df$row), stats::setNames(df$col_label, df$col))
  lab[!duplicated(names(lab))]
}
