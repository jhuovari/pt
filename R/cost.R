# Toimialojen kustannusrakenne käyttötaulukoista.

pt_use_nonproduct_rows <- c("00T99", "PUR_S2", "PUR_F", "CIF_FOB", "D21N", "FIMUSE_OH",
                            "D1K", "D11K", "D29N", "P51CK", "B13N", "B3N", "B1GPH", "P1R")

#' Toimialat ja niiden ryhmät
#'
#' @param codes Toimialakoodit (esim. `"C17"`, `"D35"`).
#' @return Merkkijonovektori: `"Alkutuotanto"`, `"Kaivostoiminta"`,
#'   `"Teollisuus"`, `"Energia- ja vesihuolto"`, `"Rakentaminen"`, `"Palvelut"`.
#' @export
pt_industry_group <- function(codes) {
  l <- substr(codes, 1, 1)
  out <- rep("Palvelut", length(codes))
  out[l == "A"] <- "Alkutuotanto"
  out[l == "B"] <- "Kaivostoiminta"
  out[l == "C"] <- "Teollisuus"
  out[l %in% c("D", "E")] <- "Energia- ja vesihuolto"
  out[l == "F"] <- "Rakentaminen"
  out
}

#' Onko toimiala teollisuutta (TOL C)
#'
#' @param codes Toimialakoodit.
#' @param mining Otetaanko mukaan kaivostoiminta (B).
#' @param energy Otetaanko mukaan energiahuolto (D35).
#' @export
pt_is_industry <- function(codes, mining = FALSE, energy = FALSE) {
  l <- substr(codes, 1, 1)
  l == "C" | (mining & l == "B") | (energy & codes == "D35")
}

#' Toimialojen kustannusrakenne
#'
#' Purkaa toimialojen tuotoksen (perushintaan) kustannuseriin:
#' välituotteet tuotteittain, tuoteverot (perushintaisessa taulukossa omana
#' eränään), palkansaajakorvaukset, muut tuotantoverot, kiinteän pääoman
#' kuluminen ja nettotoimintaylijäämä.
#'
#' Ostajanhintaisessa taulukossa (`prices = "purchaser"`) välituotteiden
#' arvoon sisältyvät tuoteverot (esim. sähkö- ja polttoaineverot) sekä kauppa-
#' ja kuljetusmarginaalit. Se kuvaa siis yrityksen maksamaa hintaa.
#' Perushintaisessa taulukossa tuoteverot ovat yhtenä rivinä `D21N`.
#'
#' @param years Vuodet.
#' @param prices `"purchaser"` (14yg) tai `"basic"` (14ym).
#' @param cache Käytetäänkö välimuistia.
#' @return data.frame: `year`, `industry`, `industry_label`, `item`,
#'   `item_label`, `type` (`"intermediate"`, `"other_intermediate"`,
#'   `"product_taxes"`, `"value_added"`), `value` (milj. e), `share`
#'   (osuus tuotoksesta perushintaan) ja `share_intermediate` (osuus
#'   välituotekäytöstä ostajanhintaan).
#' @export
pt_cost_structure <- function(years = NULL, prices = c("purchaser", "basic"), cache = TRUE) {
  prices <- match.arg(prices)
  u <- pt_get(if (prices == "purchaser") "use_purchaser" else "use_basic",
              years = years, cache = cache)
  u <- u[!(u$col %in% pt_aggregate_cols), ]
  va_items <- c("D1K", "D29N", "P51CK", "B13N")
  keep <- !(u$row %in% pt_use_nonproduct_rows) |
    u$row %in% c("PUR_S2", "CIF_FOB", "D21N", va_items)
  d <- u[keep, ]
  d$type <- ifelse(d$row %in% va_items, "value_added",
                   ifelse(d$row == "D21N", "product_taxes",
                          ifelse(d$row %in% c("PUR_S2", "CIF_FOB"),
                                 "other_intermediate", "intermediate")))
  key <- paste(u$year, u$col)
  out_x <- stats::setNames(u$value[u$row == "P1R"], key[u$row == "P1R"])
  inter <- stats::setNames(u$value[u$row == "FIMUSE_OH"], key[u$row == "FIMUSE_OH"])
  k <- paste(d$year, d$col)
  data.frame(
    year = d$year, industry = d$col, industry_label = d$col_label,
    item = d$row, item_label = d$row_label, type = d$type,
    value = d$value,
    share = safe_div(d$value, unname(out_x[k])),
    share_intermediate = ifelse(d$type == "value_added", NA_real_,
                                safe_div(d$value, unname(inter[k]))),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' Valittujen panosten osuus toimialojen kustannuksista
#'
#' @param products Nimetty vektori tuotekoodeista, esim.
#'   `c(sahko = "35", oljy = "19")`. Nimet tulevat tulokseen sarakkeeseen
#'   `input`.
#' @param years Vuodet.
#' @param cache Käytetäänkö välimuistia.
#' @return data.frame: `year`, `industry`, `industry_label`, `input`,
#'   `product`, `purchaser` (milj. e ostajanhintaan), `basic` (perushintaan),
#'   `imported` (tuonti perushintaan), `domestic` (kotimainen perushintaan),
#'   `taxes_margins` (ostajan- ja perushinnan erotus), `output` (tuotos), sekä
#'   osuudet tuotoksesta: `share_purchaser`, `share_basic`,
#'   `share_imported`, `share_domestic` ja `import_share` (tuonnin osuus
#'   perushintaisesta käytöstä).
#' @export
pt_input_shares <- function(products = c(sahko = "35", oljy = "19"),
                            years = NULL, cache = TRUE) {
  if (is.null(names(products))) names(products) <- products
  get <- function(type) {
    d <- pt_get(type, years = years, cache = cache)
    d[!(d$col %in% pt_aggregate_cols), ]
  }
  up <- get("use_purchaser")
  ub <- get("use_basic")
  um <- get("imports")
  pick <- function(d, code) {
    s <- d[d$row == code, ]
    stats::setNames(s$value, paste(s$year, s$col))
  }
  base <- ub[ub$row == "P1R", c("year", "col", "col_label", "value")]
  names(base) <- c("year", "industry", "industry_label", "output")
  key <- paste(base$year, base$industry)
  res <- lapply(names(products), function(nm) {
    code <- products[[nm]]
    p <- pick(up, code)[key]; b <- pick(ub, code)[key]; m <- pick(um, code)[key]
    p[is.na(p)] <- 0; b[is.na(b)] <- 0; m[is.na(m)] <- 0
    data.frame(base[c("year", "industry", "industry_label")], input = nm,
               product = code, purchaser = unname(p), basic = unname(b),
               imported = unname(m), domestic = unname(b - m),
               taxes_margins = unname(p - b), output = base$output,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, res)
  out$share_purchaser <- safe_div(out$purchaser, out$output)
  out$share_basic <- safe_div(out$basic, out$output)
  out$share_imported <- safe_div(out$imported, out$output)
  out$share_domestic <- safe_div(out$domestic, out$output)
  out$import_share <- safe_div(out$imported, out$basic)
  rownames(out) <- NULL
  out
}

#' Panosten kokonaiskustannusosuus (suora + välillinen)
#'
#' Laskee hintamallilla ([pt_price_model()]), kuinka suuri osa toimialan
#' tuotantokustannuksista on viime kädessä valittua panosta, kun mukaan
#' lasketaan panos, joka sisältyy muiden kotimaisten toimialojen toimittamiin
#' välituotteisiin. Laskenta tehdään perushintaan.
#'
#' @param sys `pt_system`.
#' @param domestic Kotimaiset toimialat, joiden tuotos on panos (esim. `"D35"`).
#' @param imported Tuontituotteiden koodit (esim. `"35"`).
#' @return data.frame: `industry`, `label`, `direct`, `indirect`, `total`
#'   (osuuksina tuotoksesta).
#' @export
pt_total_input_share <- function(sys, domestic = character(), imported = character()) {
  pm <- pt_price_model(sys,
                       domestic = stats::setNames(rep(1, length(domestic)), domestic),
                       imported = stats::setNames(rep(1, length(imported)), imported))
  # Eksogeenisille toimialoille (esim. energiantuottaja itse) lasketaan suora
  # osuus omasta tuotoksesta erikseen: A[E, j] + Am[m, j].
  E <- pm$exogenous
  if (any(E)) {
    j <- pm$industry[E]
    d <- colSums(sys$A[domestic, j, drop = FALSE])
    if (length(imported)) d <- d + colSums(sys$Am[imported, j, drop = FALSE])
    pm$direct[E] <- d
    pm$total[E] <- NA_real_
    pm$indirect[E] <- NA_real_
  }
  pm[c("industry", "label", "direct", "indirect", "total", "exogenous")]
}
