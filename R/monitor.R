# Monitor státní pokladny (MF ČR, monitor.statnipokladna.gov.cz) -> data/monitor_*.csv
library(tidyverse)
library(statnipokladna)

dir.create("data", showWarnings = FALSE)
options(statnipokladna.dest_dir = file.path(tempdir(), "statnipokladna"))

# Plnění státního rozpočtu, roky 2010+: skutečnost podle seskupení položek (první dvě číslice druhového třídění)
# a skupin paragrafů (první číslice odvětvového třídění). Jen kapitoly státního rozpočtu (03xx); výkaz do roku 2014
# obsahuje i státní fondy (05xx). Výkaz od 2015 má měsíční údaje (sčítají se), do 2014 kumulativní (bere se prosinec).
rok_monitor <- 2025 # poslední úplný rok
map(2010:rok_monitor, \(rok) {
  tabulka <- if (rok < 2015) "budget-central-old" else "budget-central"
  sp_get_table(tabulka, year = rok, month = 12) |>
    filter(rok >= 2015 | vykaz_month == "12", str_starts(kapitola, "03"), str_detect(polozka, "^[1-8]")) |>
    mutate(seskupeni = str_sub(polozka, 1, 2), skupina = str_sub(paragraf, 1, 1)) |>
    summarise(mil_kc = sum(budget_spending) / 1e6, .by = c(seskupeni, skupina)) |>
    mutate(rok = rok, .before = 1)
}) |>
  list_rbind() |>
  arrange(rok, seskupeni, skupina) |>
  write_csv("data/monitor_sr.csv")
