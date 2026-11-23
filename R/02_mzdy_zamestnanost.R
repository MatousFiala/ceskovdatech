library(tidyverse)
source("./R/utils.R")

zam_source <- list(list(name = "ČSÚ",
                        dataset = "Zaměstnanost a nezaměstnanost (VŠPS)",
                        url = "https://data.csu.gov.cz/datastat/data/VYBER/ZAM01T2",
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
    title = "Míra nezaměstnanosti",
    subtitle = "Obecná [míra nezaměstnanosti::Podíl lidí v pracovní síle, kteří si hledají práci, ale v danou chvíli žádnou nemají]",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "nezamestnanost", label = "Míra nezaměstnanosti", format = "percent")
    ),
    source = zam_source,
    options = list(table = list(list(field = "zamestnanost", label = "Míra zaměstnanosti", format = "percent"))),
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
    subtitle = str_glue("Obecná [míra nezaměstnanosti::Podíl lidí v pracovní síle, kteří si hledají práci, ale v danou chvíli žádnou nemají] podle kraje v roce {max(nezam_kraje$year)}"),
    aes = list(
        list(type = "geometry", field = "kraj_kod", label = "Kraj", labeller = "kraj_nazev"),
        list(type = "fill", field = "nezamestnanost", label = "Míra nezaměstnanosti", format = "percent", scheme = "reds")
    ),
    source = zam_source,
    data_note = ""
)


mzdy <- read_csv("./data/czso_MZDR.csv", guess_max = 1e5) |>
    filter(IndicatorType == "5958P",          # mzda na přepočtené počty zaměstnanců
           ZJIST == 0, ISEKTORMZDH.ISEKTOR2 == 0, KATPMZD == 0, CZNACEMZDY == "A-S",
           is.na(Uz0123vm.REGION), is.na(Uz0123vm.KRAJ)) |>
    select(year = Roky, mzda = Hodnota)

ceny <- read_csv("./data/czso_CEN0101H.csv") |>
    filter(Ukazatel == "Průměrná roční míra inflace", !str_detect(CASRMX, "-")) |>
    transmute(year = as.integer(CASRMX), inflace = Hodnota) |>
    arrange(year) |>
    mutate(ceny_2020 = cumprod(1 + inflace / 100),
           ceny_2020 = ceny_2020 / ceny_2020[year == 2020])

mzdy_realne <- mzdy |> 
    inner_join( ceny, by = "year") |> 
    mutate(mzda_realna = mzda / ceny_2020) |> 
    select(year, mzda_nominalni = mzda, mzda_realna)


write_chart(
    df = mzdy_realne,
    id = "mzdy-cr",
    type = "line",
    title = "Průměrná mzda",
    subtitle = "Průměrná hrubá měsíční mzda",
    tabs = list(
        list(id = "realna", label = "Reálná (ceny roku 2020)"),
        list(id = "nominalni", label = "Nominální")
    ),
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "mzda_realna", label = "Průměrná reálná mzda", unit = "Kč", tab = "realna"),
        list(type = "y", field = "mzda_nominalni", label = "Průměrná nominální mzda", unit = "Kč", tab = "nominalni")
    ),
    source = c(mzdy_source, list(list(name = "ČSÚ",
                                      dataset = "Míra inflace (CEN0101H)",
                                      url = "https://data.csu.gov.cz/datastat/data/VYBER/CEN0101HT01",
                                      retrieved = format(Sys.Date())))),
    data_note = "Reálné mzdy jsou přepočtené na ceny roku 2020 (inflace podle ČSÚ)."
)

# Průměrná mzda podle krajů (pracovištní metoda: podle místa pracoviště), poslední rok
mzdy_kraje <- read_csv("./data/czso_MZDR.csv", guess_max = 1e5) |>
    filter(IndicatorType == "5958P", !is.na(Uz0123vm.KRAJ)) |>
    filter(Roky == max(Roky)) |>
    select(year = Roky, kraj_kod = Uz0123vm.KRAJ, kraj_nazev = `ČR, regiony, kraje-Kraj`, mzda = Hodnota)

write_chart(
    df = mzdy_kraje,
    id = "mzdy-kraje",
    type = "map",
    title = "Průměrná mzda v krajích",
    subtitle = str_glue("Průměrná nominální hrubá měsíční mzda podle kraje v roce {max(mzdy_kraje$year)}"),
    aes = list(
        list(type = "geometry", field = "kraj_kod", label = "Kraj", labeller = "kraj_nazev"),
        list(type = "fill", field = "mzda", label = "Průměrná mzda", unit = "Kč", scheme = "greens")
    ),
    source = mzdy_source,
    data_note = "Mzda přepočtená na plné úvazky. Zaměstnanci jsou zařazeni podle místa pracoviště, nikoli podle bydliště nebo sídla zaměstnavatele."
)

# Podíl zaměstnanců podle sektoru (národní účty, sekce CZ-NACE sloučené do šesti sektorů)
sektory <- c(
    A = "Zemědělství",
    B = "Průmysl", C = "Průmysl", D = "Průmysl", E = "Průmysl",
    F = "Stavebnictví",
    G = "Obchod, doprava a pohostinství", H = "Obchod, doprava a pohostinství", I = "Obchod, doprava a pohostinství",
    O = "Veřejná správa, školství a zdravotnictví", P = "Veřejná správa, školství a zdravotnictví",
    Q = "Veřejná správa, školství a zdravotnictví"
)
zam_sektory <- read_csv("./data/czso_nu_M0132a.csv") |>
    filter(activity != "_T", !is.na(value)) |>
    mutate(sektor = coalesce(sektory[activity], "Ostatní služby")) |>
    summarise(zamestnanci = sum(value), .by = c(period, sektor)) |>
    mutate(podil = zamestnanci / sum(zamestnanci), .by = period) |>
    select(year = period, sektor, podil)

write_chart(
    df = zam_sektory,
    id = "zamestnanci-sektory",
    type = "line",
    title = "Zaměstnanci podle sektoru",
    subtitle = "Podíl zaměstnanců podle sektoru (bez [sebezaměstnaných::Podnikatelů a osob samostatně výdělečně činných])",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "podil", label = "Podíl zaměstnanců", format = "percent"),
        list(type = "colour", field = "sektor", label = "Sektor")
    ),
    source = list(list(name = "ČSÚ",
                       dataset = "Národní účty – zaměstnanci (osoby)",
                       url = "https://apl.czso.cz/pll/rocenka/rocenkavyber.makroek_pracov",
                       retrieved = format(Sys.Date()))),
    options = list(facet = TRUE),
    data_note = "Průmysl: těžba, zpracovatelský průmysl, energetika, voda a odpady. Ostatní služby: IT, finance, nemovitosti, odborné a administrativní činnosti, kultura a další. Sekce CZ-NACE: průmysl B–E; obchod, doprava a pohostinství G–I; veřejná správa, školství a zdravotnictví O–Q; ostatní služby J–N a R–T."
)

# Míra nezaměstnanosti podle věku: pětileté skupiny ČSÚ sloučené do šesti.
# Míry nejde prostě zprůměrovat, váží se velikostí pracovní síly (Eurostat, od 1997;
# pro roky 1993–1996 se použijí váhy z roku 1997).
vek_skupiny <- c(
    "15 až 19" = "15–19", "20 až 24" = "20–24",
    "25 až 29" = "25–34", "30 až 34" = "25–34",
    "35 až 39" = "35–49", "40 až 44" = "35–49", "45 až 49" = "35–49",
    "50 až 54" = "50–64", "55 až 59" = "50–64", "60 až 64" = "50–64",
    "65 a více let" = "65+"
)
pracovni_sila <- read_csv("./data/eurostat_lfsa_agan.csv") |>
    filter(!is.na(values)) |> # první roky jsou v tabulce prázdné
    transmute(year = time, vek5 = age |> str_remove("^Y") |> str_replace("-", " až "), sila = values)
pracovni_sila <- pracovni_sila |>
    filter(year == min(year)) |>
    select(-year) |>
    cross_join(tibble(year = 1993:(min(pracovni_sila$year) - 1))) |>
    bind_rows(pracovni_sila)

nezam_vek <- read_csv("./data/czso_ZAM04.csv", guess_max = 1e5) |>
    filter(str_detect(CASRQX, "^\\d+$"), POHL1 == 0, UKONVZDEL == 0, `Věkové skupiny` %in% names(vek_skupiny)) |>
    transmute(year = as.integer(CASRQX), vek5 = `Věkové skupiny`, vek = vek_skupiny[vek5], mira = Hodnota / 100) |>
    left_join(pracovni_sila, by = c("year", "vek5")) |>
    mutate(sila = if (n() == 1) 1 else sila, .by = c(year, vek)) |> # skupina z jediné řady váhu nepotřebuje
    summarise(nezamestnanost = weighted.mean(mira, sila), .by = c(vek, year)) |>
    arrange(vek, year)

write_chart(
    df = nezam_vek,
    id = "nezamestnanost-vek",
    type = "line",
    title = "Míra nezaměstnanosti podle věku",
    subtitle = "Obecná [míra nezaměstnanosti::Podíl lidí v pracovní síle, kteří si hledají práci, ale v danou chvíli žádnou nemají] podle věkových skupin",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "nezamestnanost", label = "Míra nezaměstnanosti", format = "percent"),
        list(type = "colour", field = "vek", label = "Věk")
    ),
    source = list(list(name = "ČSÚ",
                       dataset = "Specifické míry nezaměstnanosti podle věkových skupin (VŠPS)",
                       url = "https://data.csu.gov.cz/datastat/data/VYBER/ZAM04T5?vSet=3&vSel=1",
                       retrieved = format(Sys.Date())),
                  list(name = "Eurostat",
                       dataset = "Active population by sex, age and citizenship (lfsa_agan)",
                       url = "https://ec.europa.eu/eurostat/databrowser/view/lfsa_agan/default/table",
                       retrieved = format(Sys.Date()))),
    options = list(facet = TRUE),
    data_note = "ČSÚ publikuje míry za pětileté skupiny; širší skupiny jsou jejich průměr vážený velikostí pracovní síly (Eurostat). Hodnoty 1993–1996 jsou přibližné. Ve skupině 15–19 let většina lidí studuje, v pracovní síle je jich málo."
)
