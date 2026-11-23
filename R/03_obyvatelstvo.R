library(tidyverse)
source("./R/utils.R")

# Viditelné texty (title, subtitle, label) jsou zatím prázdné; návrh poznámky k datům je v komentáři.

oby_source <- function(dataset, tabulka) { # tabulka = kód tabulky v DataStatu, odkaz vede přímo na data
    list(list(name = "ČSÚ", dataset = dataset,
              url = str_glue("https://data.csu.gov.cz/datastat/data/VYBER/{tabulka}"),
              retrieved = format(Sys.Date())))
}
wobyg_source <- oby_source("Obyvatelstvo – dlouhodobé časové řady za Českou republiku", "WOBYG")
oby01b_source <- oby_source("Základní údaje o stavu a pohybu obyvatel – roční údaje", "OBY01BT01")

wobyg <- read_csv("./data/czso_WOBYG.csv")

# Počet obyvatel ČR
obyvatele <- wobyg |>
    filter(Ukazatel == "Počet obyvatel k 1. 7.") |>
    transmute(year = Roky, obyvatele = Hodnota / 1e6)

write_chart(
    df = obyvatele,
    id = "obyvatele-cr",
    type = "line",
    title = "Počet obyvatel",
    subtitle = "Počet obyvatel žijících na území dnešního Česka",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "obyvatele", label = "Počet obyvatel", unit = "mil.")
    ),
    source = wobyg_source,
    options = list(annotations = list(list(x = 1946, label = "Deportace českých Němců", align = "centre"))),
    data_note = "Na území dnešního Česka. Střední stav obyvatel k 1. 7."
)

# Růst počtu obyvatel: přirozený přírůstek a čistá migrace
prirustek <- wobyg |>
    filter(Ukazatel %in% c("Přirozený přírůstek/úbytek", "Přírůstek/úbytek stěhováním", "Počet obyvatel k 1. 7."),
           Roky >= 1947) |> # souvislá řada stěhování začíná rokem 1947 (dříve jen 1830–1863)
    select(Roky, Ukazatel, Hodnota) |> 
    pivot_wider(names_from = Ukazatel, values_from = Hodnota) |> 
    mutate(prirozeny_prirustek_pct = `Přirozený přírůstek/úbytek` / `Počet obyvatel k 1. 7.`,
migrace_pct = `Přírůstek/úbytek stěhováním` / `Počet obyvatel k 1. 7.`
    ) |> 
    pivot_longer(c(prirozeny_prirustek_pct, migrace_pct), names_to = "slozka") |> 
    arrange(Roky) |>
    transmute(year = Roky,
              slozka = if_else(slozka == "prirozeny_prirustek_pct", "Přirozený přírůstek/úbytek", "Čistá migrace"),
              prirustek = value)

write_chart(
    df = prirustek,
    id = "obyvatele-prirustek",
    type = "bar",
    title = "Populační růst",
    subtitle = "Růst obyvatelstva rozdělený na [přirozený přírůstek::Živě narození minus zemřelí] a [čistou migraci::Přistěhovalí minus vystěhovalí]",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "prirustek", label = "Přírůstek", format = "percent"),
        list(type = "fill", field = "slozka", label = "Složka růstu")
    ),
    source = wobyg_source,
    options = list(annotations = list(list(x = 2021.5, label = "Začátek války na Ukrajině"))),
    data_note = "Na území dnešního Česka." 
)

# Obyvatelstvo podle kraje, poslední rok
oby01b <- read_csv("./data/czso_OBY01B.csv", guess_max = 1e5) |>
    filter(Ukazatel == "Počet obyvatel k 31. 12.", Pohlaví == "Celkem") |>
    select(year = Roky, kod = UZ01234B, nazev = `Všechna území`, obyvatele = Hodnota)

obyvatele_kraje <- oby01b |>
    filter(str_detect(kod, "^CZ\\d{3}$"), year == max(year)) |>
    transmute(year, kraj_kod = kod, kraj_nazev = nazev, obyvatele = obyvatele / 1e3)

write_chart(
    df = obyvatele_kraje,
    id = "obyvatele-kraje",
    type = "map",
    title = "Počet obyvatel v krajích",
    subtitle = str_glue("Počet obyvatel podle kraje k 31. 12. {max(obyvatele_kraje$year)}"),
    aes = list(
        list(type = "geometry", field = "kraj_kod", label = "Kraj", labeller = "kraj_nazev"),
        list(type = "fill", field = "obyvatele", label = "Počet obyvatel", unit = "tis.", scheme = "blues")
    ),
    source = oby01b_source,
    data_note = "" 
)


od <- max(oby01b$year) - 10
do <- od + 10
zmena_orp <- oby01b |>
    filter(str_detect(kod, "^\\d{4}$") | kod == "CZ010", year %in% c(od, do)) |>
    mutate(kod = if_else(kod == "CZ010", "1000", kod), # Praha: v geo/orp.geojson jeden obvod s kódem 1000
           year = if_else(year == od, "od", "do")) |>
    pivot_wider(names_from = year, values_from = obyvatele) |>
    transmute(orp_kod = kod, orp_nazev = nazev, obyvatele_od = round(od / 1e3, 1), obyvatele_do = round(do / 1e3, 1), zmena = do / od - 1)

write_chart(
    df = zmena_orp,
    id = "obyvatele-zmena-orp",
    type = "map",
    title = "Změna počtu obyvatel v posledních 10 letech",
    subtitle = str_glue("Změna počtu obyvatel podle [SO ORP::Správní obvod obce s rozšířenou působností: území, pro které jedno větší město vykonává část státní správy (např. vydává občanské a řidičské průkazy). Česko se dělí na 206 SO ORP včetně Prahy] mezi roky {od} a {do}"),
    aes = list(
        list(type = "geometry", field = "orp_kod", label = "ORP", labeller = "orp_nazev"),
        list(type = "fill", field = "zmena", label = str_glue("Změna počtu obyvatel {od}–{do}"), format = "percent")
    ),
    source = oby01b_source,
    data_note = "Část změny způsobily přesuny obcí mezi SO ORP: 3 obce z Českého Brodu do Lysé nad Labem, 1 obec z Turnova do Jablonce nad Nisou, 1 obec z Jihlavy do Havlíčkova Brodu a 1 obec z Prostějova do Konice.",
    options = list(geo = "orp.geojson", geo_key = "KOD_ORP", midpoint = 0,
                   table = list(list(field = "obyvatele_od", label = str_glue("Obyvatelé {od}"), unit = "tis."),
                                list(field = "obyvatele_do", label = str_glue("Obyvatelé {do}"), unit = "tis.")),
                   scheme = c("#d6453d", "#ffffff", "#2e9a4f"), fill_domain = c(-0.12, 0.12))
)

# Obyvatelstvo podle velikosti obce (deset skupin ČSÚ sloučených do pěti), podíl na obyvatelstvu
velikost_skupiny <- c(
    "méně než 200" = "do 999", "200 až 499" = "do 999", "500 až 999" = "do 999",
    "1 000 až 1 999" = "1 000–4 999", "2 000 až 4 999" = "1 000–4 999",
    "5 000 až 9 999" = "5 000–19 999", "10 000 až 19 999" = "5 000–19 999",
    "20 000 až 49 999" = "20 000–99 999", "50 000 až 99 999" = "20 000–99 999",
    "100 000 a více" = "100 000 a více"
)
velikost_obci <- read_csv("./data/czso_RSO02A01.csv") |>
    filter(Ukazatel == "Počet obyvatel", Uz02 == "CZ", `Velikostní skupina obcí` %in% names(velikost_skupiny), !is.na(Hodnota), Roky >= 1990) |>
    mutate(velikost = factor(velikost_skupiny[`Velikostní skupina obcí`], levels = unique(velikost_skupiny))) |>
    summarise(obyvatele = sum(Hodnota), .by = c(velikost, Roky)) |>
    mutate(podil = obyvatele / sum(obyvatele), .by = Roky) |>
    arrange(velikost, Roky) |>
    transmute(year = Roky, velikost, obyvatele = round(obyvatele / 1e3, 1), podil)

write_chart(
    df = velikost_obci,
    id = "obyvatele-velikost-obci",
    type = "line",
    title = "Rozdělení obyvatel podle velikosti obcí",
    subtitle = "Podíl obyvatel s bydlištěm v obci dané velikostní kategorie",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "podil", label = "Podíl obyvatel", format = "percent"),
        list(type = "colour", field = "velikost", label = "Velikostní skupina obce")
    ),
    source = oby_source("Velikostní skupiny obcí", "RSO02A01T1"),
    options = list(facet = TRUE, table = list(list(field = "obyvatele", label = "Počet obyvatel", unit = "tis."))),
    data_note = "" 
)
