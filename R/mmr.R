# Ministerstvo pro místní rozvoj (dříve Agentura pro sociální začleňování) -> data/mmr_*.csv
library(tidyverse)

dir.create("data", showWarnings = FALSE)

# Index sociálního vyloučení (ISV) za obce, s body za jednotlivé dimenze a hodnotami za SO ORP a kraje, od 2016.
# Jeden Excel se všemi roky; odkaz se mění s každou aktualizací, proto se čte ze stránky.
isv_page <- "https://mmr.gov.cz/cs/microsites/socialni-zaclenovani/index-socialniho-vylouceni/data-a-mapy"
isv_url <- read_lines(isv_page) |>
  str_extract("/getmedia/[^\"]*ISV_[^\"]*\\.xlsx[^\"]*") |>
  discard(is.na) |>
  first() |>
  str_replace_all("&amp;", "&")
xlsx <- tempfile(fileext = ".xlsx")
download.file(str_c("https://mmr.gov.cz", isv_url), xlsx, mode = "wb", quiet = TRUE)
readxl::read_excel(xlsx) |>
  write_csv("data/mmr_isv.csv")
