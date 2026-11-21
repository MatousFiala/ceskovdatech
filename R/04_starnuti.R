library(tidyverse)
source("./R/utils.R")

# Stárnutí a porodnost
# Viditelné texty (title, subtitle, label) jsou zatím prázdné; návrh poznámky k datům je v komentáři.
# Všechny údaje jsou ke konci roku (31. 12.); stavy k 1. 1. (Eurostat, projekce) jsou přiřazeny předchozímu roku.
# Řady: Eurostat 1960–1999, ČSÚ 2000–poslední rok, projekce ČSÚ (střední varianta) dál.

csu_source <- function(dataset, tabulka) { # tabulka = kód tabulky v DataStatu, odkaz vede přímo na data
    list(name = "ČSÚ", dataset = dataset,
         url = str_glue("https://data.csu.gov.cz/datastat/data/VYBER/{tabulka}"),
         retrieved = format(Sys.Date()))
}
vek_source <- csu_source("Počet obyvatel podle pohlaví a jednotek věku k 31. 12.", "OBY02BHVDT01")
projekce_source <- csu_source("Projektovaný počet obyvatel podle pohlaví a jednotek věku (projekce ČSÚ 2023)", "OBY02PROJ1AT01")
slozeni_source <- csu_source("Ukazatele věkového složení k 31. 12.", "OBY02FHVDT01")
eurostat_source <- list(name = "Eurostat", dataset = "Population on 1 January by age and sex (demo_pjan)",
                        url = "https://ec.europa.eu/eurostat/databrowser/view/demo_pjan/default/table",
                        retrieved = format(Sys.Date()))
sources <- list(eurostat_source, vek_source, projekce_source)

skutecnost <- "Skutečnost"
projekce <- "Projekce ČSÚ"

# ---------- obyvatelé podle pohlaví a jednotek věku (věk 100 = 100 a více) ----------
csu_vek <- function(df) { # ČSÚ: věk "0" ... "100 a více"
    df |>
        filter(`Věk (roky)` != "Celkem", Pohlaví != "Celkem") |>
        mutate(vek = parse_number(`Věk (roky)`))
}

vek_eurostat <- read_csv("./data/eurostat_demo_pjan.csv") |>
    filter(sex != "T", !age %in% c("TOTAL", "UNK"), !is.na(values)) |>
    mutate(vek = case_when(age == "Y_LT1" ~ 0, age == "Y_OPEN" ~ NA, .default = as.numeric(str_extract(age, "\\d+")))) |>
    mutate(vek = coalesce(vek, max(vek, na.rm = TRUE) + 1), .by = c(time, sex)) |> # otevřená skupina (99+ nebo 100+)
    filter(time %in% 1961:2000) |> # dál ČSÚ (31. 12. 2000 = 1. 1. 2001); Eurostat od 1. 1. 2001 už počítá se sčítáním 2001, ČSÚ až od konce roku 2001
    transmute(year = time - 1, vek, pohlavi = if_else(sex == "M", "Muži", "Ženy"), obyvatele = values, data = skutecnost)

vek_csu <- read_csv("./data/czso_OBY02BHVD.csv") |>
    filter(Uz012 == "CZ") |>
    csu_vek() |>
    transmute(year = Rok, vek, pohlavi = Pohlaví, obyvatele = Hodnota, data = skutecnost)

posledni <- max(vek_csu$year)

vek_projekce <- read_csv("./data/czso_OBY02PROJ1A.csv") |>
    filter(`Varianta projekce` == "Střední varianta") |>
    csu_vek() |>
    transmute(year = Roky - 1, vek, pohlavi = Pohlaví, obyvatele = Hodnota, data = projekce) |>
    filter(year > posledni, year <= 2100)

vek1 <- bind_rows(vek_eurostat, vek_csu, vek_projekce) |>
    mutate(vek = pmin(vek, 100)) |>
    summarise(obyvatele = sum(obyvatele), .by = c(year, data, pohlavi, vek)) |>
    mutate(data = factor(data, c(skutecnost, projekce))) |>
    arrange(data, year, pohlavi, vek)

# Projekce navazuje na poslední skutečný rok, aby čára nebyla přerušená
navaz <- function(df) {
    bind_rows(df, filter(df, year == posledni) |> mutate(data = factor(projekce, levels(df$data)))) |>
        arrange(data, year)
}

# ---------- (1) Medián věku ----------
median_vek <- function(vek, n) { # věk v letech dokončených: osoby věku x jsou rovnoměrně v [x, x + 1)
    o <- order(vek)
    vek <- vek[o]
    n <- n[o]
    i <- which(cumsum(n) >= sum(n) / 2)[1]
    vek[i] + (sum(n) / 2 - sum(n[seq_len(i - 1)])) / n[i]
}

vek_median <- vek1 |>
    summarise(obyvatele = sum(obyvatele), .by = c(year, data, vek)) |>
    summarise(median = median_vek(vek, obyvatele), .by = c(year, data)) |>
    navaz()

write_chart(
    df = vek_median,
    id = "vek-median",
    type = "line",
    title = "Mediánový věk v ČR od roku 1960 s projekcí do roku 2100",
    subtitle = "[Medián::Polovina obyvatel je mladší, než tento věk; polivina startší] věku obyvatel, skutečnost a projekce ČSÚ",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "median", label = "Medián věku", unit = "let"),
        list(type = "linetype", field = "data", label = "Údaj")
    ),
    source = sources,
    options = list(y_zero = FALSE),
    data_note = "Před rokem 2000 použita data Eurostatu, předtím ČSÚ. Budoucí údaje jsou střední varianta projekce  ČSÚ z roku 2023." # "Medián věku: polovina obyvatel je mladší, polovina starší. Spočítán z počtu obyvatel podle jednotek věku k 31. 12. (do roku 1999 Eurostat, poté ČSÚ). Od roku 2026 střední varianta projekce ČSÚ z roku 2023."
)

# ---------- (2) Věková pyramida ----------
pyramida <- vek1 |>
    mutate(pohlavi = factor(pohlavi, c("Muži", "Ženy")), obyvatele = obyvatele / 1e3) |>
    arrange(year, pohlavi, vek) |>
    select(year, vek, pohlavi, obyvatele, data)

write_chart(
    df = pyramida,
    id = "vek-pyramida",
    type = "pyramid",
    title = "Věková pyramida ČR od roku 1960 s projekcí do roku 2100",
    subtitle = "Počet mužů a žen daného věku, skutečnost a projekce ČSÚ, tisíce",
    aes = list(
        list(type = "x", field = "vek", label = "Věk"),
        list(type = "y", field = "obyvatele", label = "Počet obyvatel", unit = "tis."),
        list(type = "fill", field = "pohlavi", label = "Pohlaví"),
        list(type = "frame", field = "year", label = "Rok", format = "year"),
        list(type = "linetype", field = "data", label = "Údaj")
    ),
    source = sources,
    options = list(reference = posledni),
    data_note = "Před rokem 2000 použita data Eurostatu, předtím ČSÚ. Budoucí údaje jsou střední varianta projekce  ČSÚ z roku 2023." # "Počet obyvatel podle pohlaví a věku k 31. 12. Poslední věková skupina zahrnuje i starší (do roku 1989 99 a více let, poté 100 a více). Od roku 2026 střední varianta projekce ČSÚ z roku 2023. Obrys ukazuje rok {posledni}."
)

# ---------- (3) Počet obyvatel podle věkových skupin ----------
skupiny <- vek1 |>
    mutate(skupina = cut(vek, c(-Inf, 14, 64, 79, Inf), labels = c("0–14", "15–64", "65–79", "80+"))) |>
    summarise(obyvatele = sum(obyvatele) / 1e6, .by = c(year, data, skupina)) |>
    navaz() |>
    arrange(skupina, data, year)

write_chart(
    df = skupiny,
    id = "vek-skupiny",
    type = "line",
    title = "Věková struktura obyvatel  ČR od roku 1960 s projekcí do roku 2100",
    subtitle = "Skutečnost a projekce ČSÚ, miliony",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "obyvatele", label = "Počet obyvatel", unit = "mil."),
        list(type = "colour", field = "skupina", label = "Věková skupina"),
        list(type = "linetype", field = "data", label = "Údaj")
    ),
    source = sources,
    data_note = "Před rokem 2000 použita data Eurostatu, předtím ČSÚ. Budoucí údaje jsou střední varianta projekce  ČSÚ z roku 2023." # "Počet obyvatel ve věkové skupině k 31. 12. Od roku 2026 střední varianta projekce ČSÚ z roku 2023."
)

# ---------- (4) Index závislosti ----------
zavislost <- vek1 |>
    summarise(deti = sum(obyvatele[vek <= 14]), aktivni = sum(obyvatele[vek %in% 15:64]),
              seniori = sum(obyvatele[vek >= 65]), .by = c(year, data)) |>
    transmute(year, data,
              `Celkem` = (deti + seniori) / aktivni,
              `Děti (0–14)` = deti / aktivni,
              `Senioři (65+)` = seniori / aktivni) |>
    pivot_longer(-c(year, data), names_to = "zavislost", values_to = "index") |>
    navaz() |>
    arrange(zavislost, data, year)

write_chart(
    df = zavislost,
    id = "vek-zavislost",
    type = "line",
    title = "Index závislosti v ČR od roku 1960 s projekcí do roku 2100",
    subtitle = "Počet [dětí::0-14 let] a [seniorů::nad 65 let] na 100 lidí v [produktivním věku::15-64 let], skutečnost a projece ČSÚ",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "index", label = "Index závislosti", format = "percent"),
        list(type = "colour", field = "zavislost", label = "Skupina"),
        list(type = "linetype", field = "data", label = "Údaj")
    ),
    source = sources,
    data_note = "Před rokem 2000 použita data Eurostatu, předtím ČSÚ. Budoucí údaje jsou střední varianta projekce  ČSÚ z roku 2023." # "Index závislosti: počet dětí (0–14 let) a seniorů (65+) na 100 lidí v produktivním věku (15–64 let), k 31. 12. Od roku 2026 střední varianta projekce ČSÚ z roku 2023."
)

# ---------- (5) Průměrný věk a index závislosti v krajích ----------
# ČSÚ publikuje průměrný věk krajů přímo (z jednotek věku), není třeba počítat z pětiletých skupin
slozeni <- read_csv("./data/czso_OBY02FHVD.csv") |>
    filter(str_detect(Uz012, "^CZ0\\d\\d$"), Pohlaví == "Celkem", Rok == max(Rok)) |>
    select(year = Rok, kraj_kod = Uz012, kraj_nazev = `ČR, Reg. soudržnosti, Kraje`, IndicatorType, Hodnota) |>
    pivot_wider(names_from = IndicatorType, values_from = Hodnota) |>
    transmute(year, kraj_kod, kraj_nazev,
              prumerny_vek = `5436`,
              zavislost = (`2406VP014` + `2406VP65AV`) / `2406VP1564`) # stejná definice jako v grafu (4)

write_chart(
    df = slozeni,
    id = "vek-kraje",
    type = "map",
    title = "Věková struktura v krajích",
    subtitle = str_glue("Průměrný věk a [index závislosti::Počet dětí (0-14 let) a seniorů (nad 65 let) na 100 lidí v produktivním věku (15-64 let)] v roce {max(slozeni$year)} po krajích"),
    tabs = list(list(id = "vek", label = "Průměrný věk"), list(id = "zavislost", label = "Index závislosti")),
    aes = list(
        list(type = "geometry", field = "kraj_kod", label = "Kraj", labeller = "kraj_nazev"),
        list(type = "fill", field = "prumerny_vek", label = "Průměrný věk", unit = "let", tab = "vek"),
        list(type = "fill", field = "zavislost", label = "Index závislosti", format = "percent", tab = "zavislost")
    ),
    source = slozeni_source,
    options = list(reverse = TRUE), # mladší / méně závislých = světlejší
    data_note = "" # "Průměrný věk obyvatel a index závislosti (děti 0–14 let a senioři 65+ na 100 lidí ve věku 15–64 let) k 31. 12. {max(slozeni$year)}."
)

# ---------- Porodnost ----------
# Řady: Eurostat do roku 2000, ČSÚ od 2001 (podle jednotek věku matky má ČSÚ data až od roku 2001)
narozeni_source <- csu_source("Narození - vybrané souhrnné údaje", "OBY03HVDT01")
narozeni_vek_source <- csu_source("Živě narození podle pořadí, jednotek věku a rodinného stavu matky", "OBY03BT02")
eurostat_find_source <- list(name = "Eurostat", dataset = "Fertility indicators (demo_find)",
                             url = "https://ec.europa.eu/eurostat/databrowser/view/demo_find/default/table",
                             retrieved = format(Sys.Date()))
eurostat_fagec_source <- list(name = "Eurostat", dataset = "Live births by mother's age (demo_fagec)",
                              url = "https://ec.europa.eu/eurostat/databrowser/view/demo_fagec/default/table",
                              retrieved = format(Sys.Date()))

find <- read_csv("./data/eurostat_demo_find.csv") |> filter(time <= 2000, !is.na(values))
oby03 <- read_csv("./data/czso_OBY03HVD.csv") |> filter(Uz012 == "CZ", Roky > 2000)

# Živě narození podle jednotek věku matky (věk 14 = do 14, 50 = 50 a více)
narozeni_vek <- bind_rows(
    read_csv("./data/eurostat_demo_fagec.csv") |>
        filter(time <= 2000, !is.na(values), str_detect(age, "^Y\\d+$|^Y10-14$|^Y_GE50$")) |>
        transmute(year = time, narozeni = values,
                  vek = case_when(age == "Y10-14" ~ 14, age == "Y_GE50" ~ 50, .default = as.numeric(str_extract(age, "\\d+")))),
    read_csv("./data/czso_OBY03B.csv") |>
        rename(vek = `Věk matky (jednoleté a pětileté skupiny)`) |>
        filter(`Pořadí narození` == "Celkem", `Rodinný stav matky` == "Celkem",
               vek != "Celkem", vek != "do 19", !str_detect(vek, " - ")) |> # jednotky věku, "do 14" a "50 a více" (ne pětileté skupiny)
        transmute(year = Roky, narozeni = Hodnota, vek = parse_number(vek))
)

# (6) Úhrnná plodnost: průměrný počet dětí na ženu
plodnost <- bind_rows(
    find |> filter(indic_de == "TOTFERRT") |> transmute(year = time, plodnost = values),
    oby03 |> filter(Ukazatel == "Úhrnná plodnost") |> transmute(year = Roky, plodnost = Hodnota)
) |>
    arrange(year)

write_chart(
    df = plodnost,
    id = "porodnost-plodnost",
    type = "line",
    title = "Míra plodnosti v ČR od roku 1960",
    subtitle = "[Úhrnná plodnost::Odhad, kolik dětí by se narodilo jedné ženě, kdyby po celý její život platily míry porodnosti z daného roku], počet dětí na ženu",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "plodnost", label = "Plodnost")
    ),
    source = list(eurostat_find_source, narozeni_source),
    options = list(annotations = list(list(y = 2.1, label = "Hranice udržení stabilní populace bez migrace"))), # label: "Prostá reprodukce (2,1)"
    data_note = "Do roku 2000 data Eurostatu, poté ČSÚ" # "Úhrnná plodnost: kolik dětí by se narodilo jedné ženě, kdyby po celý život platily míry plodnosti daného roku. Hodnota okolo 2,1 dítěte na ženu zajistí, že se generace bez migrace nahradí (prostá reprodukce). Do roku 2000 Eurostat, poté ČSÚ."
)

# (7) Průměrný věk matky při narození prvního dítěte (Eurostat má údaj od roku 1995)
prvni_vek <- bind_rows(
    find |> filter(indic_de == "AGEMOTH1") |> transmute(year = time, vek = values),
    oby03 |> filter(Ukazatel == "Průměrný věk matek při narození 1. dítěte") |> transmute(year = Roky, vek = Hodnota)
) |>
    arrange(year)

write_chart(
    df = prvni_vek,
    id = "porodnost-vek-prvni",
    type = "line",
    title = "Průměrný věk prvorodiček v ČR od roku 1995",
    subtitle = "Průměr věku, ve které má matka své první dítě",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "vek", label = "Věk", unit = "let")
    ),
    source = list(eurostat_find_source, narozeni_source),
    options = list(y_zero = FALSE),
    data_note = "Do roku 2000 data Eurostatu, poté ČSÚ" # "Průměrný věk matky při narození prvního živě narozeného dítěte. Do roku 2000 Eurostat (od roku 1995), poté ČSÚ."
)

# (8) Podíl narozených podle věku a rodinného stavu matky (taby)
vek_matky <- narozeni_vek |>
    mutate(skupina = cut(vek, c(-Inf, 19, 24, 29, 39, Inf), labels = c("do 19", "20–24", "25–29", "30–39", "40+"))) |>
    summarise(narozeni = sum(narozeni), .by = c(year, skupina)) |>
    mutate(podil_vek = narozeni / sum(narozeni), .by = year) |>
    select(year, skupina, podil_vek)

podil_narozeni <- arrange(vek_matky, skupina, year)
write_chart(
    df = podil_narozeni,
    id = "porodnost-skupiny",
    type = "line",
    title = "Věk a matek v ČR od roku 1960",
    subtitle = "Poměr dětí narozený matkám podle věkové skupiny",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "podil_vek", label = "Podíl dětí", format = "percent"),
        list(type = "colour", field = "skupina", label = "Věková skupina")
    ),
    source = list(eurostat_fagec_source, eurostat_find_source, narozeni_vek_source, narozeni_source),
    options = list(facet = TRUE),
    data_note = "Do roku 2000 data Eurostatu, poté ČSÚ" # "Podíl živě narozených dětí podle věku matky a podle toho, zda byla matka vdaná. Do roku 2000 Eurostat, poté ČSÚ."
)
