# ÚZIS (Národní zdravotnický informační portál) -> data/uzis_*.csv
library(tidyverse)

dir.create("data", showWarnings = FALSE)

# Ambulantní péče v psychiatrii (NRHZS), od 2010: pacienti podle roku, kraje zařízení, pohlaví,
# základní diagnózy a věku. https://www.nzip.cz/data/2058-ambulantni-pece-psychiatrie-otevrena-data
read_csv("https://datanzis.uzis.gov.cz/data/OIS-04-NISDZ/OIS-04-03/Otevrena-data-OIS-04-03-ambulantni-pece-psychiatrie.csv",
         show_col_types = FALSE) |>
  write_csv("data/uzis_ambulantni_psychiatrie.csv")
