library(tidyverse)
source("./R/utils.R")

zam_source <- list(list(name = "ČSÚ",
                        dataset = "Zaměstnanost a nezaměstnanost (VŠPS)",
                        url = "https://data.csu.gov.cz/datastat/info/SADA/ZAM01",
                        retrieved = format(Sys.Date())))

# Obecná míra nezaměstnanosti, roční údaje
nezam <- read_csv("./data/czso_ZAM01.csv", guess_max = 1e5) |>
    filter(IndicatorType == "6290", !str_detect(CASRQX, "Q")) |>
    transmute(year = as.integer(CASRQX), sex = Pohlaví,
              kraj_kod = Uz02h.KRAJ, kraj = `Území-Kraj`, rate = Hodnota)

# Míra nezaměstnanosti v ČR podle pohlaví
write_chart(
    df = nezam |> filter(is.na(kraj_kod)) |> select(year, sex, rate),
    id = "nezamestnanost",
    type = "line",
    title = "Míra nezaměstnanosti v Česku",
    subtitle = "Obecná míra nezaměstnanosti osob ve věku 15 a více let, roční průměr",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "rate", label = "Míra nezaměstnanosti", unit = "%"),
        list(type = "colour", field = "sex", label = "Pohlaví")
    ),
    source = zam_source,
    data_note = "Podle výběrového šetření pracovních sil; liší se od podílu nezaměstnaných osob podle úřadů práce."
)

# Míra nezaměstnanosti podle krajů, poslední rok
nezam_kraje <- nezam |>
    filter(!is.na(kraj_kod), sex == "Celkem", year == max(year)) |>
    select(year, kraj_kod, kraj, rate)

write_chart(
    df = nezam_kraje,
    id = "nezamestnanost-kraje",
    type = "bar",
    title = str_glue("Míra nezaměstnanosti podle krajů, {max(nezam_kraje$year)}"),
    subtitle = "Obecná míra nezaměstnanosti osob ve věku 15 a více let",
    aes = list(
        list(type = "x", field = "kraj", label = "Kraj"),
        list(type = "y", field = "rate", label = "Míra nezaměstnanosti", unit = "%")
    ),
    source = zam_source,
    data_note = "",
    options = list(sort = "desc")
)

write_chart(
    df = nezam_kraje,
    id = "nezamestnanost-mapa",
    type = "map",
    title = str_glue("Míra nezaměstnanosti podle krajů, {max(nezam_kraje$year)}"),
    subtitle = "Obecná míra nezaměstnanosti osob ve věku 15 a více let",
    aes = list(
        list(type = "geometry", field = "kraj_kod", labeller = "kraj", label = "Kraj"),
        list(type = "fill", field = "rate", label = "Míra nezaměstnanosti", unit = "%")
    ),
    source = zam_source,
    data_note = ""
)
