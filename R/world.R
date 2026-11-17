# World Bank + Maddison Project + WHO -> data/wdi_*.csv, data/maddison_*.csv, data/who_*.csv
library(tidyverse)
library(WDI)

dir.create("data", showWarnings = FALSE)

# HDP na osobu, PPP, stálé mezinárodní dolary (World Bank API is slow, be patient)
WDI(indicator = c(gdp_pc_ppp = "NY.GDP.PCAP.PP.KD"), start = 1990) |>
  write_csv("data/wdi_gdp_pc_ppp.csv")

# HDP na osobu od roku 1820 (Maddison Project Database, as published by Our World in Data)
read_csv("https://ourworldindata.org/grapher/gdp-per-capita-maddison-project-database.csv?csvType=full",
         guess_max = 1e5, show_col_types = FALSE) |>
  write_csv("data/maddison_gdp_pc.csv")

# WHO Global Health Observatory, Česko: mateřská úmrtnost (odhady UN MMEIG, od 1985),
# nadváha a obezita dospělých (BMI 25+ a 30+, NCD-RisC, od 1980) a kouření tabáku (od 2000, od 2025 projekce);
# nadváha, obezita a kouření jsou standardizované podle věku
who <- c(materska_umrtnost = "MDG_0000000026", nadvaha = "NCD_BMI_25A", obezita = "NCD_BMI_30A", koureni = "M_Est_smk_curr_std")
imap(who, \(code, name) {
  jsonlite::fromJSON(str_glue("https://ghoapi.azureedge.net/api/{code}?$filter=SpatialDim%20eq%20%27CZE%27"))$value |>
    as_tibble() |>
    transmute(ukazatel = name, rok = TimeDim, pohlavi = Dim1, hodnota = NumericValue, dolni = Low, horni = High)
}) |>
  list_rbind() |>
  write_csv("data/who_gho.csv")

# Sebevraždy: věkově standardizovaná úmrtnost (WHO Mortality Database, jak ji publikuje Our World in Data), od 1986
read_csv("https://ourworldindata.org/grapher/suicide-rate-who-mdb.csv?csvType=full",
         show_col_types = FALSE) |>
  write_csv("data/owid_suicide_who_mdb.csv")

# Podíl nejbohatšího 1 % na příjmech (před zdaněním) a na majetku (World Inequality Database, jak ji publikuje
# Our World in Data), všechny země a regiony
walk(c("income-share-top-1-before-tax-wid", "wealth-share-richest-1-percent"), \(slug) {
  read_csv(str_glue("https://ourworldindata.org/grapher/{slug}.csv?csvType=full"), show_col_types = FALSE) |>
    write_csv(str_glue("data/owid_{slug}.csv"))
})

# Evropská unie (World Inequality Database): Our World in Data agregát EU nemá, proto z hromadného stažení WID
# (zip 900 MB, uloží se jen řádky EU). Soubor EU má starší názvy proměnných: sptincj992 = podíl na příjmech
# před zdaněním, shwealj992 = podíl na majetku; QY-MER = EU agregovaná podle tržních kurzů (jako regiony v OWID).
options(timeout = 3600)
wid_zip <- tempfile(fileext = ".zip")
download.file("https://wid.world/bulk_download/wid_all_data.zip", wid_zip, mode = "wb", quiet = TRUE)
read_delim(unz(wid_zip, "WID_data_QY-MER.csv"), delim = ";", show_col_types = FALSE) |>
  filter(variable %in% c("sptincj992", "shwealj992"), percentile == "p99p100") |>
  write_csv("data/wid_eu.csv")
unlink(wid_zip)
