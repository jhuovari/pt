# Sähkön ja öljyn osuus teollisuuden toimialojen kustannusrakenteessa
#
# Aja paketin juurihakemistosta:
#   R CMD INSTALL .            # asenna ptfin
#   Rscript analyysi/sahko_oljy.R
#
# Tulokset: analyysi/tulokset/*.csv ja analyysi/kuvat/*.png

library(ptfin)
library(ggplot2)

options(ptfin.cache_dir = file.path("analyysi", "cache"))
dir.create(file.path("analyysi", "tulokset"), showWarnings = FALSE)
dir.create(file.path("analyysi", "kuvat"), showWarnings = FALSE)

years <- pt_years()
last_year <- max(years)

# Tuotteet (CPA 2008) ja niitä vastaavat kotimaiset toimialat.
#   35  sähkö, kaasu, lämpö ja ilmastointi   <-> toimiala D35
#   19  koksi ja jalostetut öljytuotteet      <-> toimiala C19
inputs <- c(sahko = "35", oljy = "19")
domestic_industry <- c(sahko = "D35", oljy = "C19")
input_label <- c(sahko = "Sähkö, kaasu ja lämpö (35)", oljy = "Öljytuotteet (19)")

short <- c(
  B = "Kaivostoiminta (B)", C10TC12 = "Elintarvike (10-12)", C13TC15 = "Tekstiili (13-15)",
  C16 = "Puutuote (16)", C17 = "Paperi (17)", C18 = "Painaminen (18)",
  C19 = "Öljynjalostus (19)", C20 = "Kemia (20)", C21 = "Lääke (21)",
  C22 = "Kumi ja muovi (22)", C23 = "Mineraalituote (23)", C24 = "Metallien jalostus (24)",
  C25 = "Metallituote (25)", C26 = "Elektroniikka (26)", C27 = "Sähkölaite (27)",
  C28 = "Kone (28)", C29 = "Moottoriajoneuvo (29)", C30 = "Muu kulkuneuvo (30)",
  C31_C32 = "Huonekalu ja muu (31-32)", C33 = "Korjaus ja asennus (33)",
  C = "Teollisuus pl. öljynjalostus"
)

# ------------------------------------------------------------------------------
# 1. Suorat kustannusosuudet käyttötaulukoista (ostajanhintaan ja perushintaan)
# ------------------------------------------------------------------------------
sh <- pt_input_shares(inputs, years = years)
sh <- sh[pt_is_industry(sh$industry, mining = TRUE), ]

# Teollisuus yhteensä ilman öljynjalostusta (C pl. 19) = toimialojen summa.
# Öljynjalostus jätetään pois, koska se on itse öljytuotteiden tuottaja ja sen
# oma öljytuotteiden käyttö (jalostamon sisäiset virrat) hallitsisi summaa.
ind_c <- sh[pt_is_industry(sh$industry) & sh$industry != "C19", ]
num <- c("purchaser", "basic", "imported", "domestic", "taxes_margins", "output")
tot <- aggregate(ind_c[num], ind_c[c("year", "input", "product")], sum)
tot$industry <- "C"
tot$industry_label <- "Teollisuus pl. öljynjalostus"
sh <- rbind(sh[c("year", "industry", "industry_label", "input", "product", num)],
            tot[c("year", "industry", "industry_label", "input", "product", num)])
sh$share_purchaser <- sh$purchaser / sh$output
sh$share_basic <- sh$basic / sh$output
sh$import_share <- ifelse(sh$basic > 0, sh$imported / sh$basic, NA)
sh$tax_margin_share <- ifelse(sh$purchaser > 0, sh$taxes_margins / sh$purchaser, NA)
sh$name <- short[sh$industry]

# Rakenteelliset lisätiedot: raakaöljy (tuote 05-09) öljynjalostuksen panoksena
crude <- pt_input_shares(c(kaivos = "05_09"), years = years)
crude <- crude[crude$industry == "C19", c("year", "purchaser", "output", "share_purchaser", "import_share")]

# ------------------------------------------------------------------------------
# 2. Kokonaiskustannusosuudet (suora + välillinen) hintamallilla
# ------------------------------------------------------------------------------
tot_share <- do.call(rbind, lapply(years, function(y) {
  s <- pt_system(y)
  do.call(rbind, lapply(names(inputs), function(nm) {
    t <- pt_total_input_share(s, domestic = domestic_industry[[nm]], imported = inputs[[nm]])
    # Teollisuus pl. öljynjalostus: tuotoksella painotettu keskiarvo
    w <- pt_is_industry(t$industry) & t$industry != "C19"
    agg <- data.frame(industry = "C", label = "Teollisuus pl. öljynjalostus",
                      direct = weighted.mean(t$direct[w], s$x[w]),
                      indirect = weighted.mean(t$indirect[w], s$x[w]),
                      total = weighted.mean(t$total[w], s$x[w]), exogenous = FALSE)
    t <- rbind(t, agg)
    t$year <- y
    t$input <- nm
    t
  }))
}))
tot_share <- tot_share[pt_is_industry(tot_share$industry, mining = TRUE) | tot_share$industry == "C", ]
tot_share$name <- short[tot_share$industry]

# Välittymiskanavat viimeisenä vuonna: minkä toimialojen kautta panos tulee
s_last <- pt_system(last_year)
channels <- do.call(rbind, lapply(names(inputs), function(nm) {
  dom <- stats::setNames(1, domestic_industry[[nm]])
  imp <- stats::setNames(1, inputs[[nm]])
  js <- setdiff(names(short), c("C", domestic_industry[[nm]]))
  do.call(rbind, lapply(js, function(j) {
    ch <- head(pt_price_channels(s_last, j, dom, imp), 3)
    data.frame(input = nm, industry = j, name = short[[j]], rank = 1:3,
               channel = ch$channel, channel_label = ch$label,
               contribution = ch$contribution, share = ch$share)
  }))
}))
write.csv(channels, sprintf("analyysi/tulokset/kanavat_%d.csv", last_year), row.names = FALSE)

# ------------------------------------------------------------------------------
# 3. Fyysinen energiankäyttö (teollisuuden energiankäyttötilasto)
# ------------------------------------------------------------------------------
en <- industry_energy_use(years = years)
en_w <- reshape(en[c("year", "group", "group_label", "source", "value")],
                idvar = c("year", "group", "group_label"), timevar = "source",
                direction = "wide")
names(en_w) <- sub("^value\\.", "e", names(en_w))
en_w[is.na(en_w)] <- 0
en_w <- data.frame(year = en_w$year, group = en_w$group, group_label = en_w$group_label,
                   sahko_gwh = en_w$e7, lampo_gwh = en_w$e8, oljy_gwh = en_w$e1,
                   maakaasu_gwh = en_w$e3, yhteensa_gwh = en_w$eS)
en_w$sahko_osuus <- en_w$sahko_gwh / en_w$yhteensa_gwh
en_w$oljy_osuus <- en_w$oljy_gwh / en_w$yhteensa_gwh

# Rahamääräiset tiedot energiatilaston toimialaryhmille
map <- energy_group_map()
m <- merge(sh[sh$industry != "C", ], map, by = "industry")
cost_g <- aggregate(cbind(purchaser, output) ~ year + group + input, m, sum)
cost_g <- reshape(cost_g, idvar = c("year", "group", "output"), timevar = "input", direction = "wide")
en_w <- merge(en_w, cost_g, by = c("year", "group"), all.x = TRUE)
# Karkea tarkistus: tuotteen 35 hankinnat euroina / (sähkö + lämpö) MWh
en_w$yksikkohinta_35_e_mwh <- en_w$purchaser.sahko * 1e6 / ((en_w$sahko_gwh + en_w$lampo_gwh) * 1000)
en_w$sahko_mwh_per_milj_e <- en_w$sahko_gwh * 1000 / en_w$output
en_w <- en_w[order(en_w$year, en_w$group), ]

# ------------------------------------------------------------------------------
# 4. Tallennus
# ------------------------------------------------------------------------------
pct <- function(x, d = 2) round(100 * x, d)
out1 <- sh[order(sh$input, sh$year, -sh$share_purchaser),
           c("year", "input", "industry", "name", "purchaser", "basic", "imported",
             "taxes_margins", "output", "share_purchaser", "share_basic",
             "import_share", "tax_margin_share")]
write.csv(out1, "analyysi/tulokset/suorat_osuudet.csv", row.names = FALSE)
out2 <- tot_share[c("year", "input", "industry", "name", "direct", "indirect", "total")]
write.csv(out2, "analyysi/tulokset/kokonaisosuudet.csv", row.names = FALSE)
write.csv(en_w, "analyysi/tulokset/energiankaytto.csv", row.names = FALSE)
write.csv(crude, "analyysi/tulokset/raakaoljy_oljynjalostus.csv", row.names = FALSE)

# Tiivistelmätaulukko viimeiseltä vuodelta
w <- function(d, nm, col) stats::setNames(d[[col]][d$input == nm & d$year == last_year],
                                          d$industry[d$input == nm & d$year == last_year])
codes <- c(setdiff(names(short), "C"), "C")
summary_tab <- data.frame(
  toimiala = unname(short[codes]),
  sahko_suora_oh = pct(w(sh, "sahko", "share_purchaser")[codes]),
  sahko_suora_ph = pct(w(sh, "sahko", "share_basic")[codes]),
  sahko_suora_pt = pct(w(tot_share, "sahko", "direct")[codes]),
  sahko_kokonais_ph = pct(w(tot_share, "sahko", "total")[codes]),
  oljy_suora_oh = pct(w(sh, "oljy", "share_purchaser")[codes]),
  oljy_suora_ph = pct(w(sh, "oljy", "share_basic")[codes]),
  oljy_suora_pt = pct(w(tot_share, "oljy", "direct")[codes]),
  oljy_kokonais_ph = pct(w(tot_share, "oljy", "total")[codes]),
  oljy_tuontiosuus = pct(w(sh, "oljy", "import_share")[codes], 0)
)
write.csv(summary_tab, sprintf("analyysi/tulokset/yhteenveto_%d.csv", last_year), row.names = FALSE)
print(summary_tab, row.names = FALSE)

# ------------------------------------------------------------------------------
# 5. Kuvat
# ------------------------------------------------------------------------------
col_direct <- "#2a78d6"
col_indirect <- "#9dc3ef"
theme_pt <- theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        plot.title.position = "plot", legend.position = "top",
        legend.justification = "left", plot.caption = element_text(colour = "#52514e"),
        plot.background = element_rect(fill = "white", colour = NA))
caption <- "Lähde: Tilastokeskus, panos-tuotostaulukot (14yn, 14yp, 14ym, 14yg). Laskelmat: ptfin."

bar_fig <- function(nm, file) {
  d <- tot_share[tot_share$input == nm & tot_share$year == last_year & !tot_share$exogenous, ]
  d <- d[d$industry != "C", ]
  lv <- d$name[order(d$total)]
  long <- rbind(data.frame(name = d$name, part = "Suora", value = d$direct),
                data.frame(name = d$name, part = "Välillinen (muiden toimialojen panosten kautta)",
                           value = d$indirect))
  long$name <- factor(long$name, levels = lv)
  long$part <- factor(long$part, levels = rev(unique(long$part)))
  cagg <- tot_share[tot_share$input == nm & tot_share$year == last_year & tot_share$industry == "C", ]
  p <- ggplot(long, aes(value * 100, name, fill = part)) +
    geom_vline(xintercept = cagg$total * 100, linetype = "dashed", colour = "#8a8984") +
    geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
    geom_label(data = d, aes(total * 100, factor(name, levels = lv),
                             label = sprintf("%.1f", total * 100)),
               inherit.aes = FALSE, hjust = -0.1, size = 3.2, colour = "#0b0b0b",
               fill = "white", label.size = 0, label.padding = unit(0.1, "lines")) +
    scale_fill_manual(values = c("Suora" = col_direct,
                                 "Välillinen (muiden toimialojen panosten kautta)" = col_indirect),
                      breaks = c("Suora", "Välillinen (muiden toimialojen panosten kautta)"),
                      name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(x = "% tuotoksen arvosta (perushintaan)", y = NULL,
         title = sprintf("%s: osuus teollisuuden toimialojen kustannuksista %d",
                         input_label[[nm]], last_year),
         subtitle = sprintf("Suora osuus ja välillinen osuus hintamallilla. Katkoviiva: teollisuus pl. öljynjalostus %.1f %%.",
                            cagg$total * 100),
         caption = caption) +
    theme_pt
  ggsave(file, p, width = 9, height = 6.5, dpi = 150, bg = "white")
}
bar_fig("sahko", "analyysi/kuvat/sahko_kokonaisosuus.png")
bar_fig("oljy", "analyysi/kuvat/oljy_kokonaisosuus.png")

# Kehitys vuosina: suorat osuudet ostajanhintaan
sel <- c("C", "C17", "C24", "C20", "C23", "C16", "C10TC12", "B")
d <- sh[sh$industry %in% sel, ]
d$name <- factor(short[d$industry], levels = short[sel])
d$input_lab <- input_label[d$input]
p <- ggplot(d, aes(factor(year), share_purchaser * 100, group = input_lab, colour = input_lab)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2.2) +
  facet_wrap(~name, nrow = 2) +
  scale_colour_manual(values = stats::setNames(c("#2a78d6", "#eb6834"), input_label),
                      breaks = input_label, name = NULL) +
  labs(x = NULL, y = "% tuotoksen arvosta",
       title = "Sähkön ja öljytuotteiden suora kustannusosuus ostajanhintaan",
       subtitle = "Energiakriisi näkyy vuonna 2022; sähkön osuus laski vuonna 2023 useimmilla toimialoilla alle vuoden 2021 tason",
       caption = "Lähde: Tilastokeskus, käyttötaulukko ostajanhintaan (14yg). Laskelmat: ptfin.") +
  theme_pt + theme(panel.grid.major.y = element_line(colour = "#e6e5e0"),
                   panel.grid.major.x = element_blank())
ggsave("analyysi/kuvat/kehitys.png", p, width = 10, height = 5.5, dpi = 150, bg = "white")

message("Valmis. Tulokset: analyysi/tulokset, kuvat: analyysi/kuvat")
