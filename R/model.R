# Leontiefin panos-tuotosmalli StatFinin symmetrisestä taulukosta.

# 14yn:n rivit, jotka eivät ole toimialoja vaan kirjanpitoeriä.
pt_aggregate_rows <- c("P1_USE", "P7_USE", "PUR_S2", "PUR_F", "CIF_FOB", "D21N",
                       "FIMUSE_OH", "D1K", "D29N", "P51CK", "B13N", "B1GPH", "P1R",
                       "P51K", "NKANTA", "E1")
# 14yn:n sarakkeet, jotka ovat loppukäyttöä tai summia.
pt_aggregate_cols <- c("ATX", "P3KS14", "P3KS15", "P3KS13", "P3K", "P51K", "P52K",
                       "P5K", "P6KS21", "P6KS2111", "P6KS2112", "P6KS22", "P6K",
                       "FUSE_PH", "USE_PH")
pt_final_demand_cols <- c("P3KS14", "P3KS15", "P3KS13", "P51K", "P52K", "P6K")
# Tuonnin käyttötaulukon (14yp) summarivit.
pt_import_total_rows <- c("00T99", "PUR_S2", "P7_USE_PUR_S2")

#' Teknologiakertoimet
#'
#' @param Z Välituotekäyttömatriisi (toimittaja x käyttäjä).
#' @param x Tuotosvektori.
#' @return `A = Z diag(x)^-1`; nollatuotoksen sarakkeet nollia.
#' @export
pt_coefficients <- function(Z, x) {
  inv <- ifelse(x == 0, 0, 1 / x)
  sweep(Z, 2, inv, `*`)
}

#' Leontiefin käänteismatriisi
#'
#' @param A Teknologiakerroinmatriisi.
#' @return `(I - A)^-1`.
#' @export
pt_leontief <- function(A) {
  L <- solve(diag(nrow(A)) - A)
  dimnames(L) <- dimnames(A)
  L
}

#' Ghoshin käänteismatriisi (tarjontapuolen malli)
#'
#' @param Z Välituotekäyttömatriisi.
#' @param x Tuotosvektori.
#' @return `(I - B)^-1`, missä `B = diag(x)^-1 Z` (allokaatiokertoimet).
#' @export
pt_ghosh <- function(Z, x) {
  inv <- ifelse(x == 0, 0, 1 / x)
  B <- Z * inv
  G <- solve(diag(nrow(B)) - B)
  dimnames(G) <- dimnames(Z)
  G
}

safe_div <- function(a, b) ifelse(b == 0, 0, a / b)

#' Rakenna panos-tuotosmalli
#'
#' Hakee vuoden symmetrisen panos-tuotostaulukon (14yn) ja tuonnin
#' käyttötaulukon (14yp) ja kokoaa niistä mallin. Kaikki rahamäärät ovat
#' miljoonaa euroa perushintaan, taulukkovuoden hinnoin.
#'
#' @param year Taulukkovuosi.
#' @param cache Käytetäänkö välimuistia.
#' @return `pt_system`-olio (lista), jossa mm.
#'   * `Z` kotimainen välituotekäyttö (toimiala x toimiala), `x` tuotos,
#'   * `A` kotimaiset panoskertoimet, `L` Leontiefin käänteismatriisi,
#'   * `Zm`, `Am` tuontipanokset (tuote x toimiala) ja niiden kertoimet,
#'   * `va`, `comp`, `gos`, `cfc`, `othertax`, `taxes` arvonlisäyksen erät ja
#'     välituotteiden tuoteverot, `emp` työlliset (1000 henkeä),
#'   * `fd` kotimaisten tuotteiden loppukäyttö, `fd_imports` loppukäytön tuonti.
#' @export
pt_system <- function(year, cache = TRUE) {
  io <- pt_get("iot", years = year, cache = cache)
  im <- pt_get("imports", years = year, cache = cache)
  if (!nrow(io)) stop("Vuodelle ", year, " ei l\u00f6ydy panos-tuotostaulukkoa.")

  rows <- unique(io$row)
  cols <- unique(io$col)
  inds <- setdiff(cols, pt_aggregate_cols)
  if (!identical(inds, setdiff(rows, pt_aggregate_rows))) {
    stop("Rivi- ja saraketoimialat eiv\u00e4t t\u00e4sm\u00e4\u00e4 taulukossa 14yn.")
  }
  W <- pt_matrix(io)
  rowv <- function(code) W[code, inds]

  Z <- W[inds, inds]
  x <- rowv("P1R")
  A <- pt_coefficients(Z, x)

  prods <- setdiff(unique(im$row), pt_import_total_rows)
  Wm <- pt_matrix(im)
  Zm <- Wm[prods, inds, drop = FALSE]

  structure(list(
    year = as.integer(year),
    industries = inds,
    labels = pt_labels(io),
    product_labels = pt_labels(im)[prods],
    Z = Z, x = x, A = A, L = pt_leontief(A),
    Zm = Zm, Am = pt_coefficients(Zm, x),
    imports = rowv("P7_USE"),
    taxes = rowv("D21N"),
    va = rowv("B1GPH"), comp = rowv("D1K"), gos = rowv("B13N"),
    cfc = rowv("P51CK"), othertax = rowv("D29N"), emp = rowv("E1"),
    fd = W[inds, pt_final_demand_cols],
    fd_imports = W["P7_USE", pt_final_demand_cols],
    fd_all = W[rows, pt_final_demand_cols]
  ), class = "pt_system")
}

#' @export
print.pt_system <- function(x, ...) {
  cat("Panos-tuotosmalli, vuosi", x$year, "\n")
  cat(" toimialoja:", length(x$industries), "\n")
  cat(sprintf(" tuotos %.0f milj. e, arvonlis\u00e4ys %.0f milj. e, ty\u00f6lliset %.0f tuhatta\n",
              sum(x$x), sum(x$va), sum(x$emp)))
  invisible(x)
}

#' Toimialan selite
#'
#' @param sys `pt_system`.
#' @param codes Toimiala- tai tuotekoodit.
#' @export
pt_label <- function(sys, codes) {
  unname(ifelse(codes %in% names(sys$labels), sys$labels[codes], codes))
}

#' Kertoimet: tuotos, arvonlisäys, työllisyys ja tuonti
#'
#' Tyypin I kertoimet kertovat, paljonko yhden euron (miljoonan euron)
#' loppukysyntä toimialan tuotteisiin synnyttää koko taloudessa tuotosta,
#' arvonlisäystä, työllisiä ja välituotetuontia (suorat + välilliset vaikutukset).
#'
#' @param sys `pt_system`.
#' @return data.frame toimialoittain.
#' @export
pt_multipliers <- function(sys) {
  L <- sys$L
  v <- safe_div(sys$va, sys$x)
  e <- safe_div(sys$emp * 1000, sys$x)
  m <- safe_div(sys$imports, sys$x)
  data.frame(
    industry = sys$industries,
    label = pt_label(sys, sys$industries),
    output = colSums(L),
    value_added = as.vector(v %*% L),
    employment = as.vector(e %*% L),       # henkea / milj. e
    employment_direct = e,
    imports = as.vector(m %*% L),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' Rasmussenin kytkennät ja Ghoshin eteenpäinkytkentä
#'
#' @param sys `pt_system`.
#' @return data.frame: taaksepäin- ja eteenpäinkytkentäindeksit (keskiarvo 1).
#'   Toimiala on avaintoimiala, jos molemmat ovat yli 1.
#' @export
pt_linkages <- function(sys) {
  L <- sys$L
  n <- nrow(L)
  G <- pt_ghosh(sys$Z, sys$x)
  bl <- colSums(L) / (sum(L) / n)
  fl <- rowSums(L) / (sum(L) / n)
  fg <- rowSums(G) / (sum(G) / n)
  data.frame(
    industry = sys$industries, label = pt_label(sys, sys$industries),
    backward = bl, forward = fl, forward_ghosh = fg,
    key_sector = bl > 1 & fl > 1,
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' Suljettu malli (tyyppi II)
#'
#' Lisää malliin kotitaloussektorin: rivi = kotitalouksien käytettävissä
#' oleva tulo tuotosyksikköä kohden, sarake = kotitalouksien kulutuksen
#' kotimainen rakenne.
#'
#' @param sys `pt_system`.
#' @param income_share Palkansaajakorvauksista kotitalouksille käytettäväksi
#'   tuloksi jäävä osuus (työnantajamaksujen ja välittömien verojen jälkeen).
#' @param propensity Kulutusalttius käytettävissä olevasta tulosta.
#' @return Lista: `A` (n+1 x n+1) ja `L`.
#' @export
pt_closed_system <- function(sys, income_share = 0.55, propensity = 0.9) {
  n <- length(sys$industries)
  cons_dom <- sys$fd[, "P3KS14"]
  cons_total <- sum(cons_dom) + sys$fd_imports[["P3KS14"]] + sys$fd_all["PUR_S2", "P3KS14"]
  h <- safe_div(sys$comp, sys$x) * income_share
  c_dom <- cons_dom / cons_total * propensity
  Ac <- matrix(0, n + 1, n + 1,
               dimnames = list(c(sys$industries, "HH"), c(sys$industries, "HH")))
  Ac[1:n, 1:n] <- sys$A
  Ac[1:n, n + 1] <- c_dom
  Ac[n + 1, 1:n] <- h
  list(A = Ac, L = pt_leontief(Ac))
}

#' Kysyntävaikutukset
#'
#' Laskee loppukysynnän muutoksen `f` (milj. e perushintaan, kotimaiset
#' tuotteet) vaikutukset tuotokseen, arvonlisäykseen, palkkoihin, työllisiin
#' ja tuontiin.
#'
#' @param sys `pt_system`.
#' @param f Nimetty vektori (toimialakoodi -> milj. e) tai täysi vektori.
#' @param type2 Käytetäänkö suljettua mallia (kotitalouksien kulutuksen
#'   kerrannaisvaikutukset mukana).
#' @param ... Argumentit [pt_closed_system()]:lle.
#' @return Lista: `summary` (kokonaisvaikutukset) ja `industries`
#'   (toimialoittaiset vaikutukset).
#' @export
pt_impact <- function(sys, f, type2 = FALSE, ...) {
  inds <- sys$industries
  fv <- stats::setNames(rep(0, length(inds)), inds)
  if (is.null(names(f))) {
    stopifnot(length(f) == length(inds))
    fv[] <- f
  } else {
    bad <- setdiff(names(f), inds)
    if (length(bad)) stop("Tuntemattomat toimialat: ", paste(bad, collapse = ", "))
    fv[names(f)] <- f
  }
  if (type2) {
    cl <- pt_closed_system(sys, ...)
    xx <- as.vector(cl$L %*% c(fv, 0))
    xo <- xx[seq_along(inds)]
    hh <- xx[length(xx)]
  } else {
    xo <- as.vector(sys$L %*% fv)
    hh <- NA_real_
  }
  per <- function(v) safe_div(v, sys$x) * xo
  ind <- data.frame(
    industry = inds, label = pt_label(sys, inds), demand = unname(fv),
    output = xo, value_added = per(sys$va), compensation = per(sys$comp),
    employment = per(sys$emp * 1000), imports = per(sys$imports),
    row.names = NULL, stringsAsFactors = FALSE
  )
  summary <- c(demand = sum(fv), output = sum(ind$output),
               value_added = sum(ind$value_added),
               compensation = sum(ind$compensation),
               employment = sum(ind$employment), imports = sum(ind$imports),
               household_income = hh)
  list(summary = summary, industries = ind)
}

#' Leontiefin hintamalli (kustannusvaikutukset)
#'
#' Laskee, miten panosten hintojen muutos välittyy toimialojen
#' tuotantokustannuksiin ja hintoihin, kun muiden toimialojen katteet ja
#' palkat pysyvät ennallaan (täysi kustannusten siirto, ei substituutiota).
#'
#' Olkoon `E` eksogeenisten (shokattujen) kotimaisten toimialojen ja `N`
#' muiden toimialojen joukko. Tällöin
#' `dp_N' = (dp_E' A[E,N] + dpm' Am[,N]) (I - A[N,N])^-1`.
#' Suora vaikutus on sulkulausekkeen arvo, kokonaisvaikutus sisältää myös
#' välilliset vaikutukset tuotantoketjun kautta.
#'
#' Kun hinnan muutos on 1 (100 \%), kokonaisvaikutus on panoksen
#' *kokonaiskustannusosuus* (suora + välillinen).
#'
#' @param sys `pt_system`.
#' @param domestic Nimetty vektori kotimaisten toimialojen hinnanmuutoksista
#'   (osuutena, 0.1 = +10 \%), esim. `c(D35 = 0.1)`.
#' @param imported Nimetty vektori tuontituotteiden hinnanmuutoksista
#'   tuotekoodeittain (14yp), esim. `c("35" = 0.1, "19" = 0.1)`.
#' @return data.frame: `industry`, `label`, `direct`, `total`, `indirect`.
#'   Eksogeenisten toimialojen `total` on annettu shokki.
#' @export
pt_price_model <- function(sys, domestic = numeric(), imported = numeric()) {
  inds <- sys$industries
  bad <- c(setdiff(names(domestic), inds), setdiff(names(imported), rownames(sys$Am)))
  if (length(bad)) stop("Tuntemattomat koodit: ", paste(bad, collapse = ", "))
  E <- names(domestic)
  N <- setdiff(inds, E)
  direct <- stats::setNames(rep(0, length(N)), N)
  if (length(E)) {
    direct <- direct + as.vector(crossprod(sys$A[E, N, drop = FALSE], domestic[E]))
  }
  if (length(imported)) {
    direct <- direct + as.vector(crossprod(sys$Am[names(imported), N, drop = FALSE],
                                           imported))
  }
  LN <- solve(diag(length(N)) - sys$A[N, N, drop = FALSE])
  total_N <- as.vector(crossprod(LN, direct))
  out <- data.frame(industry = inds, label = pt_label(sys, inds),
                    direct = NA_real_, total = NA_real_,
                    exogenous = inds %in% E, stringsAsFactors = FALSE)
  iN <- match(N, inds)
  out$direct[iN] <- direct
  out$total[iN] <- total_N
  if (length(E)) out$total[match(E, inds)] <- domestic[E]
  out$indirect <- out$total - out$direct
  out
}

#' Hintavaikutuksen välittymiskanavat
#'
#' Purkaa [pt_price_model()]:n kokonaisvaikutuksen toimialalle `industry`
#' niihin toimialoihin, joiden kustannusten kautta vaikutus välittyy:
#' `total_j = sum_k d_k * L_N[k, j]`, missä `d` on suora vaikutus ja
#' `L_N = (I - A[N,N])^-1`. Toimialan oma rivi sisältää suoran vaikutuksen.
#'
#' @inheritParams pt_price_model
#' @param industry Toimialakoodi, jonka hintavaikutus puretaan.
#' @return data.frame: `channel`, `label`, `contribution`, `share`
#'   (osuus kokonaisvaikutuksesta), suurimmasta pienimpään.
#' @export
pt_price_channels <- function(sys, industry, domestic = numeric(), imported = numeric()) {
  pm <- pt_price_model(sys, domestic, imported)
  N <- pm$industry[!pm$exogenous]
  if (!industry %in% N) stop("Toimiala ", industry, " on eksogeeninen tai tuntematon.")
  LN <- solve(diag(length(N)) - sys$A[N, N, drop = FALSE])
  dimnames(LN) <- list(N, N)
  d <- pm$direct[match(N, pm$industry)]
  contrib <- d * LN[, industry]
  out <- data.frame(channel = N, label = pt_label(sys, N), contribution = contrib,
                    share = contrib / sum(contrib), stringsAsFactors = FALSE)
  out <- out[order(-out$contribution), ]
  rownames(out) <- NULL
  out
}
