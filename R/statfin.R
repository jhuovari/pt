# Kevyt asiakas Tilastokeskuksen StatFin-tietokannan PxWeb-rajapintaan.
#
# Rajapinta: https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/
# GET taulukon polkuun palauttaa metatiedot (muuttujat ja sallitut arvot),
# POST JSON-kyselyllä palauttaa datan. Data pyydetään json-stat2-muodossa.
# Vastaukset tallennetaan välimuistiin, jotta analyysin ajaminen uudelleen ei
# kuormita rajapintaa ja tulokset ovat toistettavissa ilman verkkoyhteyttä.

statfin_base <- function() {
  getOption("ptfin.base_url", "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin")
}

#' Välimuistihakemisto
#'
#' Palauttaa hakemiston, johon PxWeb-vastaukset tallennetaan. Hakemiston voi
#' asettaa optiolla `ptfin.cache_dir`; `NULL` tai `FALSE` poistaa välimuistin
#' käytöstä.
#'
#' @return Hakemiston polku tai `NULL`.
#' @export
statfin_cache_dir <- function() {
  d <- getOption("ptfin.cache_dir", tools::R_user_dir("ptfin", "cache"))
  if (is.null(d) || isFALSE(d)) return(NULL)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

cache_file <- function(key) {
  d <- statfin_cache_dir()
  if (is.null(d)) return(NULL)
  file.path(d, paste0(key, ".json"))
}

hash_string <- function(x) {
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(x, f, useBytes = TRUE)
  unname(substr(tools::md5sum(f), 1, 12))
}

statfin_request <- function(url, body = NULL, retries = 4) {
  last <- NULL
  for (i in seq_len(retries)) {
    res <- tryCatch({
      r <- if (is.null(body)) {
        httr::GET(url, httr::user_agent("ptfin R package"), httr::timeout(120))
      } else {
        httr::POST(url, body = body, encode = "raw",
                   httr::content_type_json(),
                   httr::user_agent("ptfin R package"), httr::timeout(120))
      }
      if (httr::status_code(r) == 429 || httr::status_code(r) >= 500) {
        stop("HTTP ", httr::status_code(r))
      }
      httr::stop_for_status(r)
      raw <- httr::content(r, as = "raw")
      bom <- as.raw(c(0xef, 0xbb, 0xbf))
      if (length(raw) >= 3 && identical(raw[1:3], bom)) raw <- raw[-(1:3)]
      txt <- rawToChar(raw)
      Encoding(txt) <- "UTF-8"
      txt
    }, error = function(e) e)
    if (!inherits(res, "error")) return(res)
    last <- res
    # PxWeb rajoittaa kyselyjen määrää (n. 30 kyselyä / 10 s).
    Sys.sleep(2^i)
  }
  stop("StatFin-kysely ep\u00e4onnistui ", retries, " yrityksen j\u00e4lkeen: ", url,
       ": ", conditionMessage(last), call. = FALSE)
}

cached_json <- function(key, fetch) {
  f <- cache_file(key)
  if (!is.null(f) && file.exists(f)) {
    txt <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  } else {
    txt <- fetch()
    if (!is.null(f)) writeLines(txt, f, useBytes = TRUE)
  }
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

#' Hae StatFin-taulukon metatiedot
#'
#' @param table Taulukon polku StatFin-tietokannassa, esim. `"pt/14yn.px"`.
#'   Hakemistopolku (esim. `"pt/"`) palauttaa hakemiston taulukkoluettelon.
#' @param cache Käytetäänkö välimuistia.
#' @return Lista, jossa `title` ja `variables` (koodit, arvot ja selitteet).
#' @export
statfin_meta <- function(table, cache = TRUE) {
  url <- paste0(statfin_base(), "/", table)
  if (!cache) return(jsonlite::fromJSON(statfin_request(url), simplifyVector = FALSE))
  key <- paste0("meta_", gsub("[^A-Za-z0-9]", "_", table))
  cached_json(key, function() statfin_request(url))
}

#' Muuttujien arvot ja selitteet taulukkomuodossa
#'
#' @inheritParams statfin_meta
#' @return data.frame: `variable`, `variable_text`, `code`, `text`.
#' @export
statfin_variables <- function(table, cache = TRUE) {
  m <- statfin_meta(table, cache = cache)
  do.call(rbind, lapply(m$variables, function(v) {
    data.frame(variable = v$code, variable_text = v$text,
               code = unlist(v$values), text = unlist(v$valueTexts),
               stringsAsFactors = FALSE)
  }))
}

#' Tee json-stat2-kysely StatFin-taulukkoon
#'
#' @param table Taulukon polku, esim. `"pt/14yn.px"`.
#' @param selection Nimetty lista: muuttujan koodi -> valittavat arvot.
#'   Arvo `"*"` valitsee kaikki arvot. Muuttujat, joita ei mainita, valitaan
#'   kokonaan.
#' @param cache Käytetäänkö välimuistia.
#' @return Siisti data.frame: jokaiselle ulottuvuudelle koodisarake ja
#'   `<ulottuvuus>_label`-selitesarake sekä `value`.
#' @export
statfin_get <- function(table, selection = list(), cache = TRUE) {
  meta <- statfin_meta(table, cache = cache)
  codes <- vapply(meta$variables, function(v) v$code, "")
  unknown <- setdiff(names(selection), codes)
  if (length(unknown)) {
    stop("Tuntemattomat muuttujat taulukossa ", table, ": ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  query <- lapply(codes, function(code) {
    vals <- selection[[code]]
    if (is.null(vals) || identical(as.character(vals), "*")) {
      list(code = code, selection = list(filter = "all", values = list("*")))
    } else {
      list(code = code, selection = list(filter = "item", values = as.list(as.character(vals))))
    }
  })
  body <- jsonlite::toJSON(list(query = query, response = list(format = "json-stat2")),
                           auto_unbox = TRUE)
  url <- paste0(statfin_base(), "/", table)
  js <- if (cache) {
    key <- paste0("data_", gsub("[^A-Za-z0-9]", "_", table), "_", hash_string(body))
    cached_json(key, function() statfin_request(url, body))
  } else {
    jsonlite::fromJSON(statfin_request(url, body), simplifyVector = FALSE)
  }
  jsonstat_to_df(js)
}

#' Muunna json-stat2-vastaus siistiksi data.frameksi
#'
#' @param js `jsonlite::fromJSON(..., simplifyVector = FALSE)`-muodossa
#'   luettu json-stat2-objekti.
#' @return data.frame, jossa ulottuvuuksien koodit, selitteet ja `value`.
#' @export
jsonstat_to_df <- function(js) {
  dims <- unlist(js$id)
  sizes <- unlist(js$size)
  cats <- lapply(dims, function(d) {
    cat <- js$dimension[[d]]$category
    idx <- cat$index
    codes <- if (!is.null(names(idx))) names(idx)[order(unlist(idx))] else unlist(idx)
    lab <- cat$label
    labels <- if (is.null(lab)) codes else vapply(codes, function(k) {
      if (is.null(lab[[k]])) k else lab[[k]]
    }, "")
    list(codes = codes, labels = unname(labels))
  })
  n <- prod(sizes)
  vals <- js$value
  if (!is.null(names(vals))) {           # harva muoto: {"indeksi": arvo}
    dense <- rep(NA_real_, n)
    dense[as.integer(names(vals)) + 1L] <- vapply(vals, function(v) if (is.null(v)) NA_real_ else as.numeric(v), 0)
    vals <- dense
  } else {
    vals <- vapply(vals, function(v) if (is.null(v)) NA_real_ else as.numeric(v), 0)
  }
  stopifnot(length(vals) == n)
  # json-stat: viimeinen ulottuvuus vaihtelee nopeimmin, expand.grid:ssä ensimmäinen.
  grid <- expand.grid(rev(lapply(cats, function(x) seq_along(x$codes))),
                      KEEP.OUT.ATTRS = FALSE)
  grid <- grid[rev(seq_along(dims))]
  out <- list()
  for (i in seq_along(dims)) {
    out[[dims[i]]] <- cats[[i]]$codes[grid[[i]]]
    out[[paste0(dims[i], "_label")]] <- cats[[i]]$labels[grid[[i]]]
  }
  out$value <- vals
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}
