# World Bank + Maddison Project + WHO -> data/wdi_*.csv, data/maddison_*.csv, data/who_*.csv
library(tidyverse)
library(WDI)

dir.create("data", showWarnings = FALSE)

# HDP na osobu, PPP, stálé mezinárodní dolary (World Bank API is slow, be patient)
WDI(indicator = c(gdp_pc_ppp = "NY.GDP.PCAP.PP.KD"), start = 1990) |>
  write_csv("data/wdi_gdp_pc_ppp.csv")

# Inflace v ČR (spotřebitelské ceny, roční průměr, %), od 1992: prodloužení řady ČSÚ (CEN0101H začíná rokem 1997)
WDI(country = "CZ", indicator = c(inflace = "FP.CPI.TOTL.ZG"), start = 1991) |>
  write_csv("data/wdi_inflace_cz.csv")

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

# Znečištění ovzduší: průměrná koncentrace PM2.5 vážená počtem obyvatel, µg/m³ (SatPM, Washington University
# in St. Louis, satelitní odhady, jak je publikuje Our World in Data), od 1998
read_csv("https://ourworldindata.org/grapher/pm25-air-pollution.csv?csvType=full",
         show_col_types = FALSE) |>
  write_csv("data/owid_pm25_air_pollution.csv")

# MMF (DataMapper API), všechny země: výdaje vládního sektoru (% HDP, Fiscal Monitor), hrubý dluh vládního
# sektoru (% HDP, World Economic Outlook, i agregáty jako svět) a HDP v USD (váhy pro světový průměr výdajů).
# Roky po posledním skutečném jsou odhady a projekce MMF.
c("G_X_G01_GDP_PT", "GGXWDG_NGDP", "NGDPD") |>
  map(\(ind) {
    jsonlite::fromJSON(str_glue("https://www.imf.org/external/datamapper/api/v1/{ind}"))$values[[ind]] |>
      imap(\(roky, kod) tibble(ukazatel = ind, kod = kod, rok = as.integer(names(roky)), hodnota = unlist(roky))) |>
      list_rbind()
  }) |>
  list_rbind() |>
  write_csv("data/imf_datamapper.csv")

# MMF World Economic Outlook (SDMX API), Česko: hrubý dluh vládního sektoru v Kč a % HDP včetně projekce
weo_csv <- tempfile(fileext = ".csv")
download.file("https://api.imf.org/external/sdmx/3.0/data/dataflow/IMF.RES/WEO/+/CZE.GGXWDG+GGXWDG_NGDP.A", weo_csv,
              headers = c(Accept = "application/vnd.sdmx.data+csv;version=1.0.0"), quiet = TRUE)
read_csv(weo_csv, show_col_types = FALSE) |>
  transmute(kod = COUNTRY, ukazatel = INDICATOR, rok = as.integer(TIME_PERIOD), hodnota = OBS_VALUE) |>
  write_csv("data/imf_weo_cze_dluh.csv")
unlink(weo_csv)

# OECD Global Revenue Statistics (SDMX API): daňové příjmy vládního sektoru podle druhu daně (DPFO 1100, DPPO 1200,
# majetkové 4000, DPH a jiné všeobecné daně ze zboží a služeb 5110, spotřební 5121, pojistné 2000, celkem _T) v národní měně (mld.), % HDP a % všech daní;
# Česko a srovnávané země (Estonsko: nejnižší majetkové daně v OECD), průměr OECD
oecd_url <- str_c("https://sdmx.oecd.org/public/rest/data/OECD.CTP.TPS,DSD_REV_COMP_GLOBAL@DF_RSGLOBAL,/",
                  "CZE+DEU+AUT+SVK+POL+FRA+GBR+SWE+DNK+FIN+NOR+USA+EST+OECD_REP.TAX_REV.S13.",
                  "T_1100+T_1200+T_4000+T_5110+T_5121+T_2000+_T._T.XDC+PT_B1GQ+PT_OTR_SECTOR.A?format=csvfile")
read_csv(oecd_url, show_col_types = FALSE) |>
  transmute(kod = REF_AREA, dan = STANDARD_REVENUE, jednotka = UNIT_MEASURE, rok = TIME_PERIOD, hodnota = OBS_VALUE) |>
  filter(!is.na(hodnota)) |>
  write_csv("data/oecd_revenue_statistics.csv")

# Podíl nejbohatšího 1 % na příjmech (před zdaněním) a na majetku (World Inequality Database, jak ji publikuje
# Our World in Data), všechny země a regiony
walk(c("income-share-top-1-before-tax-wid", "wealth-share-richest-1-percent"), \(slug) {
  read_csv(str_glue("https://ourworldindata.org/grapher/{slug}.csv?csvType=full"), show_col_types = FALSE) |>
    write_csv(str_glue("data/owid_{slug}.csv"))
})

# Giniho koeficient disponibilních příjmů (po zdanění a transferech, World Inequality Database) pro vybrané země,
# EU a svět, z hromadného stažení WID (zip 900 MB, uloží se jen tyto řádky). Země mají proměnnou gdiinc992j,
# EU (QY-MER, agregát podle tržních kurzů) a svět (WO) starší název gdiincj992.
options(timeout = 3600)
wid_zip <- tempfile(fileext = ".zip")
download.file("https://wid.world/bulk_download/wid_all_data.zip", wid_zip, mode = "wb", quiet = TRUE)
c("CZ", "SK", "PL", "HU", "SI", "DE", "SE", "US", "BR", "QY-MER", "WO") |>
  map(\(kod) read_delim(unz(wid_zip, str_glue("WID_data_{kod}.csv")), delim = ";",
                        col_types = cols(.default = "c"))) |>
  list_rbind() |>
  filter(variable %in% c("gdiinc992j", "gdiincj992"), percentile == "p0p100") |>
  write_csv("data/wid_gini.csv")
unlink(wid_zip)
