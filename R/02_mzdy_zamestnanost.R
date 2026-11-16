library(tidyverse)
source("./R/utils.R")

zam_source <- list(list(name = "ČSÚ",
                        dataset = "Zaměstnanost a nezaměstnanost (VŠPS)",
                        url = "https://data.csu.gov.cz/datastat/info/SADA/ZAM01",
                        retrieved = format(Sys.Date())))

mzdy_source <- list(list(name = "ČSÚ",
                         dataset = "Průměrné hrubé měsíční mzdy (čtvrtletní zjišťování)",
                         url = "https://data.csu.gov.cz/datastat/data/VYBER/MZDRT1?vSet=1&vSel=1",
                         retrieved = format(Sys.Date())))

zam_data <- read_csv("./data/czso_ZAM01.csv")  |> 
    filter(str_detect(CASRQX, "^\\d+$"), POHL1 == 0)

# Míra zaměstnanosti a nezaměstnanosti
zam <-  zam_data |> 
    filter(Ukazatel %in% c("Obecná míra nezaměstnanosti (%)", "Míra zaměstnanosti (%)"), is.na(`Území-Kraj`)) |> 
    select(CASRQX, Hodnota, Ukazatel) |> 
    pivot_wider(names_from = Ukazatel, values_from = Hodnota, values_fn = ~.x/100) |> 
    select(year = CASRQX, zamestnanost = `Míra zaměstnanosti (%)`, nezamestnanost = `Obecná míra nezaměstnanosti (%)`)

write_chart(
    df = zam,
    id = "nezamestnanost-cr",
    type = "line",
    title = "Míra nezaměstnanosti v ČR od roku 1993",
    subtitle = "Obecná [míra nezaměstnanosti::Procento lidí v pracovní síle, kteří si hledají práci ale v danou chvíli žádnou nemají. Nezapočítává ty, kteří pracovat nemohou nebo si práci nehledají], %",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "nezamestnanost", label = "Míra nezaměstnanosti", format = "percent")
    ),
    source = zam_source,
    data_note = ""
)

write_chart(
    df = zam,
    id = "zamestnanost-cr",
    type = "line",
    title = "Míra Zaměstnanosti v ČR od roku 1993",
    subtitle = "Podíl všech obyvatel ČR, kteří aktivně pracují - exkluduje nezaměstnané a ekonomicky neaktivní (důchodce, děti)",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "zamestnanost", label = "Míra zaměstnanosti", format = "percent")
    ),
    source = zam_source,
    data_note = ""
)

# Nezaměstnanost v krajích
nezam_kraje <- zam_data |> 
    filter(!is.na(Uz02h.KRAJ), CASRQX == max(CASRQX), Ukazatel == "Obecná míra nezaměstnanosti (%)") |> 
    mutate(Hodnota = Hodnota / 100) |> 
    select(year = CASRQX, kraj_kod = Uz02h.KRAJ, kraj_nazev = `Území-Kraj`, nezamestnanost = Hodnota)

write_chart(
    df = nezam_kraje,
    id = "nezamestnanost-kraje",
    type = "map",
    title = "Míra nezaměstnanosti v krajích",
    subtitle = "Obecná [míra nezaměstnanosti::Procento lidí v pracovní síle, kteří si hledají práci ale v danou chvíli žádnou nemají. Nezapočítává ty, kteří pracovat nemohou nebo si práci nehledají], %",
    aes = list(
        list(type = "geometry", field = "kraj_kod", label = "Kraj", labeller = "kraj_nazev"),
        list(type = "fill", field = "nezamestnanost", label = "Míra nezaměstnanosti", format = "percent")
    ),
    source = zam_source,
    data_note = ""
)


