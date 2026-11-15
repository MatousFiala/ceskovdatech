# World Bank + Maddison Project -> data/wdi_*.csv, data/maddison_*.csv
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
