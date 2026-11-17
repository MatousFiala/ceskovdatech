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
  "ZAM04",   # míra nezaměstnanosti podle věku a vzdělání: roky, čtvrtletí
  "ZAMG08",  # průměrná mzda a medián mezd
  "MZDR",    # průměrná mzda a počet zaměstnanců, ročně (odvětví, kraje)
  "CEN0101H", # míra inflace (pro přepočet mezd na reálné), roční od 1997
  "OBY01B",  # stav a pohyb obyvatel ročně: kraje, okresy, SO ORP (velký soubor)
  "RSO02A01", # počet obyvatel podle velikostních skupin obcí
  "OBY02BHVD", # obyvatelé podle pohlaví a jednotek věku k 31. 12., od 2000 (ČR, kraje)
  "OBY02FHVD", # ukazatele věkového složení k 31. 12. (průměrný věk, medián, indexy závislosti), od 2000
  "OBY02PROJ1A", # projekce obyvatelstva ČSÚ 2023: obyvatelé podle pohlaví a jednotek věku k 1. 1., 2024–2101
  "OBY03HVD",  # narození: souhrnné údaje (úhrnná plodnost, podíl mimo manželství), od 2000
  "OBY03B",    # živě narození podle pořadí, věku a rodinného stavu matky, od 2001
  "OBY04BCR",  # úmrtnostní tabulky za ČR (naděje dožití), od 1920
  "OBY04H"     # zemřelí kojenci do 1 roku, od 2001
)

walk(datasets, \(id) write_csv(czso_csv(id), str_glue("data/czso_{id}.csv")))

# Velké datové sady (stovky MB až 1 GB): ukládají se jen řádky, které grafy potřebují
czso_csv("OBY04BOR") |> # úmrtnostní tabulky za SO ORP, pětiletá období: jen naděje dožití při narození
  filter(Ukazatel == "Naděje dožití (ex)", `Věk (roky)` == 0) |>
  write_csv("data/czso_OBY04BOR.csv")
czso_csv("OBY01B01") |> # stav a pohyb obyvatel v obcích: jen počet obyvatel obcí k 1. 7., od 2016 (váhy pro ISV)
  filter(Ukazatel == "Počet obyvatel k 1. 7.", Pohlaví == "Celkem", str_detect(UZ24596C, "^\\d{6}$"), Roky >= 2016) |>
  write_csv("data/czso_OBY01B01.csv")
czso_csv("OBY04C") |> # zemřelí podle zkráceného seznamu příčin smrti, od 2000: jen ČR, všechny věky
  filter(UZ01234B == "CZ", VEK5C == "VEKC") |>
  write_csv("data/czso_OBY04C.csv")

# Pohyb obyvatel ČR od roku 1919, relativní údaje (kojenecká úmrtnost ad.): tabulka III z publikace
# "Počet a pohyb obyvatel České republiky v letech 1919–2023". Novější vydání má v adrese jiný rok.
pohyb_url <- "https://csu.gov.cz/produkty/pocet-a-pohyb-obyvatel-ceske-republiky-v-letech-1919-2023"
pohyb_xlsx <- read_lines(pohyb_url) |>
  str_extract("/docs/[^\"]*1300672403\\.xlsx") |>
  discard(is.na) |>
  first()
xlsx <- tempfile(fileext = ".xlsx")
download.file(str_c("https://csu.gov.cz", pohyb_xlsx), xlsx, mode = "wb", quiet = TRUE)
readxl::read_excel(xlsx, skip = 7, na = ".", .name_repair = "minimal",
                   col_names = c("rok", "snatky", "rozvody", "narozeni", "potraty", "zemreli", "podil_mimo_manzelstvi",
                                 "potraty_na_100_narozenych", "mrtvorozenost", "kojenecka_umrtnost",
                                 "novorozenecka_umrtnost", "perinatalni_umrtnost")) |>
  filter(str_detect(rok, "^\\d{4}$")) |>
  mutate(across(everything(), as.numeric)) |>
  write_csv("data/czso_pohyb_1919.csv")

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
  "M0131c", # zaměstnanost celkem, odpracované hodiny (v tisících)
  "M0132a"  # zaměstnanci (bez sebezaměstnaných), osoby
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

# Finanční situace domácností (HFCS): čisté jmění podle kvintilů čistého jmění, tabulka 12 z publikace
# "Finanční situace domácností 2022 až 2024". Novější vydání má v adrese jiné roky.
hfcs_url <- "https://csu.gov.cz/produkty/financni-situace-domacnosti-2022-az-2024-"
hfcs_xlsx <- read_lines(hfcs_url) |>
  str_extract("/docs/[^\"]*1600772512\\.xlsx") |>
  discard(is.na) |>
  first()
xlsx <- tempfile(fileext = ".xlsx")
download.file(str_c("https://csu.gov.cz", hfcs_xlsx), xlsx, mode = "wb", quiet = TRUE)
hfcs <- readxl::read_excel(xlsx, col_names = FALSE, .name_repair = "minimal")
hfcs_roky <- as.integer(unlist(hfcs[6, 2:7]))
hfcs[7:12, 1:7] |>
  set_names("kvintil", str_c(rep(c("prumer", "median"), each = 3), "_", hfcs_roky)) |>
  pivot_longer(-kvintil, names_to = c("statistika", "rok"), names_sep = "_", values_transform = as.numeric) |>
  mutate(rok = as.integer(rok)) |>
  write_csv("data/czso_hfcs_jmeni_kvintily.csv")
