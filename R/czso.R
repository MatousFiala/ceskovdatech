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

# ---------- Národní účty (apl.czso.cz/pll/rocenka), not available in DataStat ----------
nu <- "https://apl.czso.cz/pll/rocenka/"

# Zaměstnanost podle národních účtů (vnitrostátní koncept, podle odvětví `activity`; "_T" = celkem).
# Jmenovatel produktivity práce: HDP / osoby, HDP / odpracované hodiny.
nu_tables <- c(
  "M0131a", # zaměstnanost celkem, osoby
  "M0131c"  # zaměstnanost celkem, odpracované hodiny (v tisících)
)
# The export fails ("Out of range.") for years the database does not have yet,
# so the last available year is read from the table's selection form.
nu_last <- read_lines(str_c(nu, "rocenkavyber.makroek_pracov")) |>
  str_extract("(?<=value=\"?)(19|20)\\d{2}") |>
  as.integer() |>
  max(na.rm = TRUE)
walk(nu_tables, \(id) {
  str_glue("{nu}rocenka.presmsocas?mylang=CZ&priznak=M00013%25&typ=2&jak=1",
           "&jmeno_tabulka={id}&rokod=1990&rokdo={nu_last}&vystup=CSV") |>
    read_csv(na = ":", show_col_types = FALSE) |>
    write_csv(str_glue("data/czso_nu_{id}.csv"))
})

# Ukazatele produktivity práce: an Excel file, each sheet a wide table with years in columns.
# Its name ends with the year of the release, so the link is read from the page.
# Beware: the file is updated less often than the national accounts (see README).
xlsx_url <- read_lines(str_c(nu, "rocenkavyber.ukaz_prod")) |>
  str_extract("https?://[^\"']*Produktivita_prace[^\"']*\\.xlsx") |>
  discard(is.na) |>
  first()
xlsx <- tempfile(fileext = ".xlsx")
download.file(xlsx_url, xlsx, mode = "wb", quiet = TRUE)

readxl::excel_sheets(xlsx) |>
  set_names() |>
  map(\(sheet) {
    raw <- readxl::read_excel(xlsx, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
    years <- which(map_lgl(seq_len(nrow(raw)), \(i) sum(as.numeric(raw[i, ]) > 1900, na.rm = TRUE) > 5))[1]
    cols <- c("nace", "odvetvi", as.character(as.integer(as.numeric(raw[years, -(1:2)]))))
    raw |>
      set_names(cols) |>
      slice(-seq_len(years)) |>
      filter(!is.na(nace), !is.na(.data[[cols[3]]])) |>
      pivot_longer(-c(nace, odvetvi), names_to = "year", values_to = "value",
                   names_transform = as.integer, values_transform = as.numeric) |>
      mutate(title = raw[[1]][1], .before = 1)
  }) |>
  suppressWarnings() |>
  list_rbind(names_to = "sheet") |>
  write_csv("data/czso_produktivita_prace.csv")
