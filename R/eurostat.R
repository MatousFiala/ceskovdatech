# Eurostat -> data/eurostat_*.csv
library(tidyverse)
library(eurostat)

dir.create("data", showWarnings = FALSE)

tables <- c(
  "nama_10_lp_ulc", # produktivita práce na osobu a na hodinu
  "une_rt_a",       # míra nezaměstnanosti, roční
  "lfsi_emp_a"      # míra zaměstnanosti, roční
)

walk(tables, \(id) {
  get_eurostat(id, time_format = "num", cache = FALSE) |>
    write_csv(str_glue("data/eurostat_{id}.csv"))
})

# Pracovní síla podle věku (váhy pro slučování věkových skupin nezaměstnanosti).
# Only Czechia: the whole table is far larger than the ones above.
get_eurostat("lfsa_agan", filters = list(geo = "CZ", sex = "T", citizen = "TOTAL"),
             time_format = "num", cache = FALSE) |>
  write_csv("data/eurostat_lfsa_agan.csv")

# Obyvatelé ČR podle pohlaví a jednotek věku k 1. 1., od 1960 (prodloužení řady ČSÚ, která začíná rokem 2000)
get_eurostat("demo_pjan", filters = list(geo = "CZ", sex = c("T", "M", "F")), time_format = "num", cache = FALSE) |>
  write_csv("data/eurostat_demo_pjan.csv")

# Porodnost ČR od 1960: úhrnná plodnost, průměrný věk matky při prvním porodu (od 1995) a podíl narozených
# mimo manželství (demo_find), živě narození podle jednotek věku matky (demo_fagec)
walk(c("demo_find", "demo_fagec"), \(id) {
  get_eurostat(id, filters = list(geo = "CZ"), time_format = "num", cache = FALSE) |>
    write_csv(str_glue("data/eurostat_{id}.csv"))
})

# Subjektivně vnímané zdraví podle příjmového kvintilu (EU-SILC), od 2005
get_eurostat("hlth_silc_10", filters = list(geo = "CZ"), time_format = "num", cache = FALSE) |>
  write_csv("data/eurostat_hlth_silc_10.csv")

# Chudoba a nerovnost (EU-SILC), Česko a EU27: míra ohrožení chudobou (relativní a s hranicí z roku 2008),
# materiální a sociální deprivace, náklady na bydlení nad 40 % příjmu,
# neschopnost čelit nečekanému výdaji, vytápět byt a dovolit si maso ob den, Giniho koeficient
walk(c("ilc_li02", "ilc_li22b", "ilc_mdsd11", "ilc_lvho07a", "ilc_mdes04", "ilc_mdes01", "ilc_mdes03", "ilc_di12"), \(id) {
  get_eurostat(id, filters = list(geo = c("CZ", "EU27_2020")), time_format = "num", cache = FALSE) |>
    write_csv(str_glue("data/eurostat_{id}.csv"))
})

# Veřejné finance, sektor vládních institucí (S13), všechny země: celkové výdaje a příjmy (% HDP),
# hrubý dluh (% HDP a mil. národní měny)
get_eurostat("gov_10a_main", filters = list(sector = "S13", na_item = c("TE", "TR"), unit = "PC_GDP"),
             time_format = "num", cache = FALSE) |>
  write_csv("data/eurostat_gov_10a_main.csv")
get_eurostat("gov_10dd_edpt1", filters = list(sector = "S13", na_item = "GD", unit = c("PC_GDP", "MIO_NAC")),
             time_format = "num", cache = FALSE) |>
  write_csv("data/eurostat_gov_10dd_edpt1.csv")
# Česko: nominální HDP (B1GQ, mil. Kč), jmenovatel pro schodek státního rozpočtu
get_eurostat("gov_10dd_edpt1", filters = list(geo = "CZ", sector = "S1", na_item = "B1GQ", unit = "MIO_NAC"),
             time_format = "num", cache = FALSE) |>
  filter(!is.na(values)) |>
  write_csv("data/eurostat_gov_10dd_edpt1_cz.csv")
