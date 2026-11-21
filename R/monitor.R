# Monitor státní pokladny (MF ČR, monitor.statnipokladna.gov.cz) -> data/monitor_*.csv
library(tidyverse)
library(statnipokladna)

dir.create("data", showWarnings = FALSE)
options(statnipokladna.dest_dir = file.path(tempdir(), "statnipokladna"))

# Plnění státního rozpočtu, roky 2010+: skutečnost podle seskupení položek (první dvě číslice druhového třídění)
# a pododdílů paragrafů (první tři číslice odvětvového třídění; první číslice = skupina). Jen kapitoly státního rozpočtu (03xx); výkaz do roku 2014
# obsahuje i státní fondy (05xx). Výkaz od 2015 má měsíční údaje (sčítají se), do 2014 kumulativní (bere se prosinec).
rok_monitor <- 2025 # poslední úplný rok
map(2010:rok_monitor, \(rok) {
  tabulka <- if (rok < 2015) "budget-central-old" else "budget-central"
  sp_get_table(tabulka, year = rok, month = 12) |>
    filter(rok >= 2015 | vykaz_month == "12", str_starts(kapitola, "03"), str_detect(polozka, "^[1-8]")) |>
    mutate(seskupeni = str_sub(polozka, 1, 2), pododdil = str_sub(paragraf, 1, 3)) |>
    summarise(mil_kc = sum(budget_spending) / 1e6, .by = c(seskupeni, pododdil)) |>
    mutate(rok = rok, .before = 1)
}) |>
  list_rbind() |>
  arrange(rok, seskupeni, pododdil) |>
  write_csv("data/monitor_sr.csv")
