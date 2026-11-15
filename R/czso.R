# ČSÚ (DataStat, data.csu.gov.cz) -> data/czso_*.csv
library(tidyverse)
library(czso2)

dir.create("data", showWarnings = FALSE)

# Full dataset as CSV. Stand-in for czso2::czso_download_dataset(), which
# currently saves the dataset's metadata instead of its data.
czso_csv <- function(id) {
  read_csv(str_glue("https://data.csu.gov.cz/opendata/sady/{id}/distribuce/csv"),
           guess_max = 1e5, show_col_types = FALSE)
}

datasets <- c(
  "WNUC03C", # HDP ve srovnatelných cenách roku 2020, 1990+
  "NUC06R",  # HDP: běžné ceny, reálný růst, na obyvatele, kraje
  "WOBYG",   # obyvatelstvo, dlouhé časové řady (pro HDP na osobu)
  "ZAM01",   # zaměstnanost a nezaměstnanost: roky, čtvrtletí, kraje
  "ZAMG08"   # průměrná mzda a medián mezd
)

walk(datasets, \(id) write_csv(czso_csv(id), str_glue("data/czso_{id}.csv")))

# Medián mezd podle krajů exists only as a predefined selection (výběr).
# format = "CSV" because the default JSON-STAT parsing mixes up the values.
czso_get_table("WREG0404", format = "CSV") |>
  read_csv(show_col_types = FALSE) |>
  write_csv("data/czso_WREG0404.csv")
