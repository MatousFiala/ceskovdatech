library(tidyverse)
source("./R/utils.R")

# Zdraví a naděje dožití
# Viditelné texty (title, subtitle, label) jsou zatím prázdné; návrh poznámky k datům je v komentáři.

csu_source <- function(dataset, tabulka) { # tabulka = kód tabulky v DataStatu, odkaz vede přímo na data
    list(name = "ČSÚ", dataset = dataset,
         url = str_glue("https://data.csu.gov.cz/datastat/data/VYBER/{tabulka}"),
         retrieved = format(Sys.Date()))
}
other_source <- function(name, dataset, url) list(name = name, dataset = dataset, url = url, retrieved = format(Sys.Date()))

# ---------- (1) Naděje dožití, od roku 1920 ----------
# ČSÚ publikuje úmrtnostní tabulky jen zvlášť pro muže a ženy. Obě pohlaví dohromady = jedna generace narozených
# s obvyklým poměrem pohlaví (51,4 % chlapců): v každém věku vážený průměr podle počtu dožívajících (lx).
# Výsledek se od celkové naděje dožití Eurostatu liší nejvýš o 0,3 roku (v posledních letech o méně než 0,05).
podil_chlapcu <- 0.514
vek_doziti <- read_csv("./data/czso_OBY04BCR.csv") |>
    filter(Ukazatel %in% c("Tabulkový počet dožívajících (lx)", "Naděje dožití (ex)"),
           `Věk (roky)` %in% c("0", "10", "25", "45", "65", "80")) |>
    transmute(year = Roky, vek = as.numeric(`Věk (roky)`), pohlavi = Pohlaví,
              ukazatel = if_else(str_detect(Ukazatel, "lx"), "lx", "ex"), Hodnota) |>
    pivot_wider(names_from = ukazatel, values_from = Hodnota) |>
    mutate(vaha = if_else(pohlavi == "muži", podil_chlapcu, 1 - podil_chlapcu) * lx) |>
    summarise(nadeje = weighted.mean(ex, vaha), .by = c(year, vek)) |>
    mutate(vek_umrti = vek + nadeje) # očekávaný věk dožití (ne zbývající roky)

nadeje <- vek_doziti |>
    filter(vek == 0) |>
    select(year, nadeje) |>
    arrange(year)

write_chart(
    df = nadeje,
    id = "zdravi-nadeje-doziti",
    type = "line",
    title = "",
    subtitle = "",
    aes = list(
        list(type = "x", field = "year", label = "", format = "year"),
        list(type = "y", field = "nadeje", label = "", unit = "let")
    ),
    source = csu_source("Úmrtnostní tabulky za ČR", "OBY04BCRT02"),
    options = list(y_zero = FALSE),
    data_note = "" # "Naděje dožití při narození: kolik let by v průměru prožil novorozenec, kdyby po celý jeho život platila úmrtnost daného roku. Muži a ženy dohromady (ČSÚ publikuje jen zvlášť; spojeno v poměru pohlaví při narození). Území dnešní ČR."
)

# Očekávaný věk dožití podle dosaženého věku
vek_popisky <- c(`0` = "při narození", `10` = "v 10 letech", `25` = "ve 25 letech",
                 `45` = "ve 45 letech", `65` = "v 65 letech", `80` = "v 80 letech")
nadeje_vek <- vek_doziti |>
    transmute(year, vek = factor(vek_popisky[as.character(vek)], vek_popisky), vek_umrti) |>
    arrange(vek, year)

write_chart(
    df = nadeje_vek,
    id = "zdravi-nadeje-vek",
    type = "line",
    title = "",
    subtitle = "",
    aes = list(
        list(type = "x", field = "year", label = "", format = "year"),
        list(type = "y", field = "vek_umrti", label = "", unit = "let"),
        list(type = "colour", field = "vek", label = "")
    ),
    source = csu_source("Úmrtnostní tabulky za ČR", "OBY04BCRT02"),
    options = list(y_zero = FALSE),
    data_note = "" # "Očekávaný věk dožití člověka, který se dožil daného věku: věk + naděje dožití v tomto věku, při úmrtnosti daného roku. Muži a ženy dohromady."
)

# ---------- (2) Naděje dožití podle SO ORP ----------
# Naděje dožití nezávisí na věkové struktuře obyvatel (počítá se z měr úmrtnosti v jednotlivých letech věku),
# takže regiony s rozdílně starým obyvatelstvem jsou srovnatelné i bez standardizace.
orp <- read_csv("./data/czso_OBY04BOR.csv") |>
    filter(`Roky (pětileté období)` == max(`Roky (pětileté období)`)) |>
    transmute(obdobi = `Roky (pětileté období)`, orp_kod = as.character(Uz4), orp_nazev = `SO ORP`,
              pohlavi = Pohlaví, nadeje = Hodnota) |>
    pivot_wider(names_from = pohlavi, values_from = nadeje) |>
    mutate(nadeje = podil_chlapcu * muži + (1 - podil_chlapcu) * ženy) |> # muži a ženy dohromady, jako v grafu (1)
    arrange(orp_nazev)

write_chart(
    df = orp,
    id = "zdravi-nadeje-orp",
    type = "map",
    title = "",
    subtitle = "",
    aes = list(
        list(type = "geometry", field = "orp_kod", label = "", labeller = "orp_nazev"),
        list(type = "fill", field = "nadeje", label = "", unit = "let")
    ),
    source = csu_source("Naděje dožití v SO ORP a Praze", "OBY04BORT01"),
    options = list(geo = "orp.geojson", geo_key = "KOD_ORP"),
    data_note = "" # "Naděje dožití při narození v letech {orp$obdobi[1]} (pětileté období, aby výsledky malých obvodů nebyly zatížené náhodou), muži a ženy dohromady (ČSÚ publikuje jen zvlášť; spojeno v poměru pohlaví při narození)."
)

# ---------- (3) Subjektivně vnímané zdraví podle příjmu (EU-SILC) ----------
kvintily <- c(QU1 = "1. kvintil (nejnižší příjmy)", QU2 = "2. kvintil", QU3 = "3. kvintil",
              QU4 = "4. kvintil", QU5 = "5. kvintil (nejvyšší příjmy)")
silc <- read_csv("./data/eurostat_hlth_silc_10.csv") |>
    filter(levels == "VG_G", sex == "T", age == "Y_GE16", quant_inc %in% names(kvintily), !is.na(values)) |>
    transmute(year = time, kvintil = factor(kvintily[quant_inc], kvintily), dobre_zdravi = values / 100) |>
    arrange(kvintil, year)

write_chart(
    df = silc,
    id = "zdravi-silc-prijem",
    type = "line",
    title = "",
    subtitle = "",
    aes = list(
        list(type = "x", field = "year", label = "", format = "year"),
        list(type = "y", field = "dobre_zdravi", label = "", format = "percent"),
        list(type = "colour", field = "kvintil", label = "")
    ),
    source = other_source("Eurostat", "Self-perceived health by sex, age and income quintile (hlth_silc_10)",
                          "https://ec.europa.eu/eurostat/databrowser/view/hlth_silc_10/default/table"),
    data_note = "" # "Podíl lidí ve věku 16 a více let, kteří své zdraví hodnotí jako dobré nebo velmi dobré, podle pětiny (kvintilu) ekvivalizovaného příjmu domácnosti. Šetření EU-SILC (Životní podmínky). V nižších kvintilech je víc starších lidí, část rozdílu tedy jde na vrub věku."
)

# ---------- (4) Kojenecká úmrtnost, od roku 1919 ----------
# Do roku 2000 tabulka ČSÚ 1919–2023, od roku 2001 DataStat: zemřelí do 1 roku / živě narození
kojenci <- read_csv("./data/czso_OBY04H.csv") |>
    filter(Pohlaví == "Celkem", `Věk (dny)` == "Fiktivní položka", `Věk (měsíce)` != "Fiktivní položka") |>
    summarise(zemreli = sum(Hodnota), .by = Roky)
narozeni <- read_csv("./data/czso_OBY03HVD.csv") |>
    filter(Uz012 == "CZ", Ukazatel == "Živě narození") |>
    select(Roky, narozeni = Hodnota)

kojenecka <- bind_rows(
    read_csv("./data/czso_pohyb_1919.csv") |>
        filter(rok <= 2000) |>
        transmute(year = rok, umrtnost = kojenecka_umrtnost / 1000),
    inner_join(kojenci, narozeni, by = "Roky") |>
        transmute(year = Roky, umrtnost = zemreli / narozeni)
) |>
    filter(!is.na(umrtnost)) |>
    arrange(year)

write_chart(
    df = kojenecka,
    id = "zdravi-kojenecka-umrtnost",
    type = "line",
    title = "",
    subtitle = "",
    aes = list(
        list(type = "x", field = "year", label = "", format = "year"),
        list(type = "y", field = "umrtnost", label = "", format = "percent")
    ),
    source = list(
        other_source("ČSÚ", "Pohyb obyvatel České republiky v letech 1919–2023 – relativní údaje",
                     "https://csu.gov.cz/produkty/pocet-a-pohyb-obyvatel-ceske-republiky-v-letech-1919-2023"),
        csu_source("Zemřelí kojenci do 1 roku", "OBY04HT02"),
        csu_source("Narození - vybrané souhrnné údaje", "OBY03HVDT01")
    ),
    data_note = "" # "Kojenecká úmrtnost: podíl živě narozených dětí, které zemřely do jednoho roku věku. Území dnešní ČR."
)

# ---------- (5) Mateřská úmrtnost ----------
# Studie za léta 1978–1997 uvádí v abstraktu jen krajní hodnoty (29 a 6 na 100 000 živě narozených),
# roční řada dostupná není. Odhady WHO/UN začínají rokem 1985, takže řada je jen jejich.
who <- read_csv("./data/who_gho.csv")
materska <- who |>
    filter(ukazatel == "materska_umrtnost") |>
    transmute(year = rok, umrtnost = hodnota, dolni, horni) |>
    arrange(year)

write_chart(
    df = materska,
    id = "zdravi-materska-umrtnost",
    type = "line",
    title = "",
    subtitle = "",
    aes = list(
        list(type = "x", field = "year", label = "", format = "year"),
        list(type = "y", field = "umrtnost", label = "")
    ),
    source = other_source("WHO", "Maternal mortality ratio (per 100 000 live births), UN MMEIG estimates",
                          "https://www.who.int/data/gho/data/indicators/indicator-details/GHO/maternal-mortality-ratio-(per-100-000-live-births)"),
    data_note = "" # "Mateřská úmrtnost: počet žen zemřelých v souvislosti s těhotenstvím, porodem nebo do 42 dnů po něm na 100 000 živě narozených dětí. Modelové odhady WHO a OSN (UN MMEIG); případů je v Česku několik ročně, proto jde o odhad s nejistotou."
)

# ---------- (6) a (7) Příčiny smrti ----------
priciny <- read_csv("./data/czso_OBY04C.csv") |>
    filter(Pohlaví == "Celkem") |>
    select(year = Roky, kod = PRICSMRTVYB1, zemreli = Hodnota) |>
    pivot_wider(names_from = kod, values_from = zemreli, values_fill = 0) |>
    filter(I00TI99 > 0) # poslední rok má zatím jen celkový počet zemřelých

# Šest skupin, podíl na všech zemřelých v čase
skupiny <- priciny |>
    transmute(
        year,
        # pořadí: nepřenosné nemoci, infekční nemoci, ostatní úmrtí
        `Nemoci srdce a cév` = I00TI99,
        `Rakovina a jiné nádory` = C00TD48,
        `Ostatní nepřenosné nemoci` = `0` - I00TI99 - C00TD48 - J09TJ11 - J12TJ18 - U07U10 - A00TB99 - V01TY98,
        `Infekce dýchací soustavy (vč. covidu)` = J09TJ11 + J12TJ18 + U07U10, # chřipka, záněty plic, covid-19
        `Ostatní infekční nemoci` = A00TB99,
        `Úrazy, nehody, sebevraždy a násilí` = V01TY98
    ) |>
    pivot_longer(-year, names_to = "pricina", values_to = "zemreli") |>
    mutate(podil = zemreli / sum(zemreli), .by = year) |>
    mutate(pricina = fct_inorder(pricina)) |>
    arrange(pricina, year)

write_chart(
    df = skupiny,
    id = "zdravi-priciny-smrti",
    type = "line",
    title = "",
    subtitle = "",
    aes = list(
        list(type = "x", field = "year", label = "", format = "year"),
        list(type = "y", field = "podil", label = "", format = "percent"),
        list(type = "colour", field = "pricina", label = "")
    ),
    source = csu_source("Zemřelí podle pohlaví, pětiletých věkových skupin a zkráceného seznamu příčin smrti", "OBY04CT01"),
    data_note = "" # "Podíl na všech zemřelých podle základní příčiny smrti (MKN-10): srdce a cévy I00–I99, nádory C00–D48, infekce dýchací soustavy: chřipka a záněty plic (J09–J18) a covid-19 (U07, U10), ostatní infekční A00–B99, vnější příčiny V01–Y98. Ostatní nepřenosné nemoci zahrnují i chronické nemoci dýchací soustavy (např. CHOPN) a nepřesně určené příčiny (R00–R99)."
)

# Počet zemřelých podle příčiny (skládaný sloupcový graf)
write_chart(
    df = skupiny |> transmute(year, pricina, zemreli = zemreli / 1e3),
    id = "zdravi-priciny-pocet",
    type = "bar",
    title = "",
    subtitle = "",
    aes = list(
        list(type = "x", field = "year", label = "", format = "year"),
        list(type = "y", field = "zemreli", label = "", unit = "tis."),
        list(type = "fill", field = "pricina", label = "")
    ),
    source = csu_source("Zemřelí podle pohlaví, pětiletých věkových skupin a zkráceného seznamu příčin smrti", "OBY04CT01"),
    data_note = "" # "Počet zemřelých podle základní příčiny smrti, v tisících. Skupiny jako v grafu podílů."
)

# ---------- (8) Nadváha, obezita a kouření ----------
obezita_koureni <- who |>
    filter(ukazatel %in% c("nadvaha", "obezita", "koureni"), pohlavi == "SEX_BTSX", rok <= 2024) |> # od 2025 projekce WHO
    transmute(year = rok,
              ukazatel = factor(ukazatel, c("nadvaha", "obezita", "koureni"),
                                c("Nadváha a obezita (BMI 25 a více)", "Obezita (BMI 30 a více)", "Kouření tabáku")),
              podil = hodnota / 100) |>
    arrange(ukazatel, year)

write_chart(
    df = obezita_koureni,
    id = "zdravi-obezita-koureni",
    type = "line",
    title = "",
    subtitle = "",
    aes = list(
        list(type = "x", field = "year", label = "", format = "year"),
        list(type = "y", field = "podil", label = "", format = "percent"),
        list(type = "colour", field = "ukazatel", label = "")
    ),
    source = list(
        other_source("WHO", "Prevalence of overweight among adults, BMI ≥ 25, age-standardized (NCD-RisC)",
                     "https://www.who.int/data/gho/data/indicators/indicator-details/GHO/prevalence-of-overweight-among-adults-bmi-=-25-(age-standardized-estimate)-(-)"),
        other_source("WHO", "Prevalence of obesity among adults, BMI ≥ 30, age-standardized (NCD-RisC)",
                     "https://www.who.int/data/gho/data/indicators/indicator-details/GHO/prevalence-of-obesity-among-adults-bmi-=-30-(age-standardized-estimate)-(-)"),
        other_source("WHO", "Estimate of current tobacco smoking prevalence, age-standardized",
                     "https://www.who.int/data/gho/data/indicators/indicator-details/GHO/gho-tobacco-control-monitor-current-tobaccosmoking-prevalence-ages-15plus-age-standardized")
    ),
    data_note = "" # "Modelové odhady WHO, standardizované podle věku. Nadváha a obezita: dospělí (18+) s indexem tělesné hmotnosti 25 a více, obezita 30 a více (NCD-RisC). Kouření: lidé ve věku 15+, kteří kouří jakýkoli tabákový výrobek (denně i příležitostně)."
)

# ---------- (9) Pacienti ambulantní psychiatrie podle základní diagnózy ----------
diagnozy <- c(
    "F32–F33" = "Deprese",
    "F40–F49 bez F42" = "Úzkostné poruchy", "F42" = "Úzkostné poruchy", # neurotické, stresové a obsedantní poruchy
    "F00 a G30" = "Demence a Alzheimerova nemoc",
    "F10" = "Závislosti", "F11–F19" = "Závislosti", # alkohol, drogy a jiné látky
    "F70–F79" = "Poruchy s nástupem v dětství a dospívání", "F80–F89" = "Poruchy s nástupem v dětství a dospívání", "F90–F99" = "Poruchy s nástupem v dětství a dospívání" # mentální retardace, autismus, ADHD ad.
)
psychiatrie <- read_csv("./data/uzis_ambulantni_psychiatrie.csv") |>
    mutate(diagnoza = factor(coalesce(diagnozy[zakladni_diagnoza], "Ostatní"), c(unique(diagnozy), "Ostatní"))) |>
    summarise(pacienti = sum(pocet_pacientu) / 1e3, .by = c(rok, diagnoza)) |>
    transmute(year = rok, diagnoza, pacienti) |>
    arrange(diagnoza, year)

write_chart(
    df = psychiatrie,
    id = "zdravi-psychiatrie",
    type = "line",
    title = "",
    subtitle = "",
    aes = list(
        list(type = "x", field = "year", label = "", format = "year"),
        list(type = "y", field = "pacienti", label = "", unit = "tis."),
        list(type = "colour", field = "diagnoza", label = "")
    ),
    source = other_source("ÚZIS", "Ambulantní péče v psychiatrii (NRHZS)",
                          "https://www.nzip.cz/data/2058-ambulantni-pece-psychiatrie-otevrena-data"),
    data_note = "" # "Počet pacientů ambulantní psychiatrické péče hrazené z veřejného zdravotního pojištění podle základní diagnózy (MKN-10): deprese F32–F33, úzkostné poruchy F40–F49, demence F00–F03 a Alzheimerova nemoc G30, závislosti F10–F19, poruchy s nástupem v dětství a dospívání F70–F99 (mentální retardace, poruchy psychického vývoje včetně autismu, ADHD a poruchy chování a emocí se začátkem v dětství a dospívání). Ostatní: hlavně schizofrenie, bipolární porucha, poruchy příjmu potravy a osobnosti. Jen lidé v péči psychiatra; růst odráží i lepší dostupnost péče. Poslední rok se může ještě mírně změnit."
)

# ---------- (10) Sebevraždy ----------
sebevrazdy <- read_csv("./data/owid_suicide_who_mdb.csv") |>
    filter(Code == "CZE") |>
    select(year = Year, sebevrazdy = 4) |> # 4. sloupec: věkově standardizovaná míra na 100 000
    arrange(year)

write_chart(
    df = sebevrazdy,
    id = "zdravi-sebevrazdy",
    type = "line",
    title = "",
    subtitle = "",
    aes = list(
        list(type = "x", field = "year", label = "", format = "year"),
        list(type = "y", field = "sebevrazdy", label = "")
    ),
    source = other_source("WHO", "Age-standardized suicide rate per 100 000 (WHO Mortality Database, via Our World in Data)",
                          "https://ourworldindata.org/grapher/suicide-rate-who-mdb"),
    data_note = "" # "Sebevraždy: počet na 100 000 obyvatel, standardizováno podle věku (WHO Mortality Database)."
)
