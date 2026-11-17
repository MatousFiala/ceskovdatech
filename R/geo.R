# Hranice krajů (Eurostat GISCO, NUTS 3) -> geo/kraje.geojson
# NUTS_ID (CZ010, CZ020, ...) matches the kraj codes used by ČSÚ.
library(tidyverse)
library(jsonlite)

dir.create("geo", showWarnings = FALSE)

nuts <- fromJSON("https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_03M_2021_4326_LEVL_3.geojson",
                 simplifyVector = FALSE)
nuts$features <- keep(nuts$features, \(f) f$properties$CNTR_CODE == "CZ")

write_json(nuts, "geo/kraje.geojson", auto_unbox = TRUE, digits = 5)

# Správní obvody ORP (balíček RCzechia, podle ČÚZK) -> geo/orp.geojson
# KOD_ORP matches the ORP codes used by ČSÚ; Praha is one unit with the code 1000.
# Simplified to about 400 m, which is below one pixel at the size the maps are drawn.
library(sf)

RCzechia::orp_polygony() |>
  ungroup() |>
  select(KOD_ORP, NAZ_ORP) |>
  st_transform(5514) |>
  st_simplify(dTolerance = 400) |>
  st_transform(4326) |>
  st_write("geo/orp.geojson", delete_dsn = TRUE, quiet = TRUE, layer_options = "COORDINATE_PRECISION=4")
