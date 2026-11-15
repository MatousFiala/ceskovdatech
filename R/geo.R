# Hranice krajů (Eurostat GISCO, NUTS 3) -> geo/kraje.geojson
# NUTS_ID (CZ010, CZ020, ...) matches the kraj codes used by ČSÚ.
library(tidyverse)
library(jsonlite)

dir.create("geo", showWarnings = FALSE)

nuts <- fromJSON("https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_03M_2021_4326_LEVL_3.geojson",
                 simplifyVector = FALSE)
nuts$features <- keep(nuts$features, \(f) f$properties$CNTR_CODE == "CZ")

write_json(nuts, "geo/kraje.geojson", auto_unbox = TRUE, digits = 5)
