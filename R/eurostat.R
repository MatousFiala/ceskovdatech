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
