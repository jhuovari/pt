# ptfin – Tilastokeskuksen panos-tuotostaulukot R:ssä

`ptfin` hakee panos-tuotosaineiston Tilastokeskuksen StatFin-tietokannasta
(PxWeb-rajapinta) ja tarjoaa funktiot panos-tuotosanalyysiin. Paketti on
R-versio aiemmasta Python-toteutuksesta (`jhuovari/datakeskus`: `statfin.py`,
`io_model.py`).

## Asennus

```sh
R CMD INSTALL .
# tai: remotes::install_github("jhuovari/pt")
```

Riippuvuudet: `httr`, `jsonlite` (analyysin kuviin `ggplot2`).

## Aineisto

| Funktio | Taulukko | Sisältö |
|---|---|---|
| `pt_get("iot")` | 14yn | Symmetrinen panos-tuotostaulukko perushintaan (toimiala × toimiala, kotimaiset tuotteet) |
| `pt_get("use_basic")` | 14ym | Käyttötaulukko perushintaan (tuote × toimiala, kotimaiset + tuonti) |
| `pt_get("use_purchaser")` | 14yg | Käyttötaulukko ostajanhintaan (sis. tuoteverot ja marginaalit) |
| `pt_get("imports")` | 14yp | Tuonnin käyttötaulukko perushintaan |
| `industry_energy_use()` | tene/11wy | Teollisuuden energiankäyttö (GWh) energialähteittäin |
| `statfin_get(table, selection)` | mikä tahansa | Yleinen json-stat2-kysely StatFiniin |

Vastaukset tallennetaan välimuistiin (`tools::R_user_dir("ptfin", "cache")`,
vaihda: `options(ptfin.cache_dir = "polku")`, poista käytöstä: `FALSE`).

## Analyysifunktiot

| Funktio | Mitä laskee |
|---|---|
| `pt_system(year)` | Malli: `Z`, `x`, `A`, Leontiefin käänteismatriisi `L`, tuontipanokset `Am`, arvonlisäyksen erät, työlliset, loppukäyttö |
| `pt_coefficients()`, `pt_leontief()`, `pt_ghosh()` | Perusmatriisit |
| `pt_multipliers(sys)` | Tuotos-, arvonlisäys-, työllisyys- ja tuontikertoimet |
| `pt_linkages(sys)` | Rasmussenin taaksepäin- ja eteenpäinkytkennät, Ghosh, avaintoimialat |
| `pt_impact(sys, f, type2)` | Kysyntävaikutukset (tyyppi I ja II) |
| `pt_price_model(sys, domestic, imported)` | Leontiefin hintamalli: kustannusshokin suora ja välillinen välittyminen |
| `pt_price_channels(sys, industry, ...)` | Minkä toimialojen kautta hintavaikutus välittyy |
| `pt_total_input_share(sys, domestic, imported)` | Panoksen kokonaiskustannusosuus (suora + välillinen) |
| `pt_cost_structure(years, prices)` | Toimialojen kustannusrakenne tuote- ja arvonlisäyserittäin |
| `pt_input_shares(products, years)` | Valittujen tuotteiden osuus kustannuksista (ostajan- ja perushintaan, kotimainen/tuonti) |

```r
library(ptfin)
pt_years()                              # 2021 2022 2023
s <- pt_system(2023)
head(pt_multipliers(s))
pt_impact(s, c(F = 100))$summary        # 100 milj. e rakentamiseen
# 10 % sähkön hinnannousu (kotimainen D35 + tuonti 35)
pt_price_model(s, domestic = c(D35 = 0.1), imported = c("35" = 0.1))
pt_input_shares(c(sahko = "35", oljy = "19"), years = 2021:2023)
```

## Analyysi: sähkö ja öljy teollisuuden kustannusrakenteessa

`Rscript analyysi/sahko_oljy.R` → tulokset `analyysi/tulokset/`, kuvat
`analyysi/kuvat/`, raportti [`analyysi/raportti.md`](analyysi/raportti.md).

## Testit

```sh
R CMD build . && R CMD check ptfin_*.tar.gz   # verkkotestit ohitetaan ilman yhteyttä
```
