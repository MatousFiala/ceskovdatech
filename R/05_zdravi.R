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
    title = "Naděje dožití v ČR od roku 1920",
    subtitle = "[Naděje dožití při narození::Kolik let by v průměru prožil novorozenec, kdyby po celý život platila úmrtnost daného roku]",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "nadeje", label = "Naděje dožití", unit = "let")
    ),
    source = csu_source("Úmrtnostní tabulky za ČR", "OBY04BCRT02"),
    options = list(y_zero = FALSE),
    data_note = "Vážený průměr mužů a žen na základě poměru pohlaví -- 1028 chlapců na 1000 dívek. ČSÚ publikuje údaje jen odděleně." # "Naděje dožití při narození: kolik let by v průměru prožil novorozenec, kdyby po celý jeho život platila úmrtnost daného roku. Muži a ženy dohromady (ČSÚ publikuje jen zvlášť; spojeno v poměru pohlaví při narození). Území dnešní ČR."
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
    title = "Naděje dožití podle věku v ČR od roku 1920",
    subtitle = "Podmíněná naděje dožití: kolika let se v průměruru dožije člověk daného věku",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "vek_umrti", label = "Naděje dožití", unit = "let"),
        list(type = "colour", field = "vek", label = "Ve věku", unit = "let")
    ),
    source = csu_source("Úmrtnostní tabulky za ČR", "OBY04BCRT02"),
    options = list(y_zero = FALSE, facet = TRUE),
    data_note = "Vážený průměr mužů a žen na základě poměru pohlaví -- 1028 chlapců na 1000 dívek. ČSÚ publikuje údaje jen odděleně." # "Očekávaný věk dožití člověka, který se dožil daného věku: věk + naděje dožití v tomto věku, při úmrtnosti daného roku. Muži a ženy dohromady."
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
    title = "Naděje dožití podle SO ORP",
    subtitle = str_glue("Naděje dožití při narození v obdobi {max(orp$obdobi)}"),
    aes = list(
        list(type = "geometry", field = "orp_kod", label = "ORP", labeller = "orp_nazev"),
        list(type = "fill", field = "nadeje", label = "Naděje dožití", unit = "let")
    ),
    source = csu_source("Naděje dožití v SO ORP a Praze", "OBY04BORT01"),
    options = list(geo = "orp.geojson", geo_key = "KOD_ORP"),
    data_note = "Data udávaná za delší období pro zmírnění statistického šumu. Vážený průměr mužů a žen na základě poměru pohlaví -- 1028 chlapců na 1000 dívek. ČSÚ publikuje údaje jen odděleně." # "Očekávaný věk dožití člověka, který se dožil daného věku: věk + naděje dožití v tomto věku, při úmrtnosti daného roku. Muži a ženy dohromady."
)

# ---------- (3) Subjektivně vnímané zdraví podle příjmu (EU-SILC) ----------
kvintily <- c(QU1 = "1. kvintil (nejnižší příjmy)", QU2 = "2. kvintil", QU3 = "3. kvintil",
              QU4 = "4. kvintil", QU5 = "5. kvintil (nejvyšší příjmy)")
silc_vse <- read_csv("./data/eurostat_hlth_silc_10.csv") |>
    filter(levels == "VG_G", sex == "T", quant_inc %in% names(kvintily), !is.na(values)) |>
    mutate(kvintil = factor(kvintily[quant_inc], kvintily))

# Očištění o věk (přímá standardizace): podíly v sedmi věkových skupinách každého kvintilu
# převážené věkovou strukturou všech obyvatel 16+ v daném roce (Eurostat, 1. 1.)
skupiny <- c("Y16-24", "Y25-34", "Y35-44", "Y45-54", "Y55-64", "Y65-74", "Y_GE75")
vahy <- read_csv("./data/eurostat_demo_pjan.csv") |>
    filter(sex == "T", str_detect(age, "^Y\\d+$|Y_OPEN"), !is.na(values)) |>
    mutate(vek = if_else(age == "Y_OPEN", 100, as.numeric(str_extract(age, "\\d+")))) |>
    filter(vek >= 16) |>
    mutate(age = skupiny[findInterval(vek, c(16, 25, 35, 45, 55, 65, 75))]) |>
    summarise(obyvatele = sum(values), .by = c(time, age)) |>
    mutate(vaha = obyvatele / sum(obyvatele), .by = time)

silc <- full_join(
    silc_vse |>
        filter(age == "Y_GE16") |>
        transmute(year = time, kvintil, dobre_zdravi = values / 100),
    silc_vse |>
        inner_join(vahy, by = c("time", "age")) |>
        summarise(dobre_zdravi_vek = sum(values / 100 * vaha), n = n(), .by = c(time, kvintil)) |>
        filter(n == length(skupiny)) |>
        transmute(year = time, kvintil, dobre_zdravi_vek),
    by = c("year", "kvintil")
) |>
    arrange(kvintil, year)


write_chart(
    df = filter(silc, year == max(year)),
    id = "zdravi-silc-prijem",
    type = "bar",
    title = "Zdravotní stav podle příjmů v ČR",
    subtitle = str_glue("Procento dotázaných hodnotící své zdraví jako 'dobré' nebo 'velmi dobré' z roku {max(silc$year)}, očištěno od vlivu nižších příjmů ve stáří"),
    tabs = list(list(id = "ocisteno", label = "Očištěno od věku"), list(id = "skutecnost", label = "Neočištěno")),
    aes = list(
        list(type = "x", field = "kvintil", label = "Kvintil příjmů"),
        list(type = "y", field = "dobre_zdravi_vek", label = "Dobré zdraví", format = "percent", tab = "ocisteno"),
        list(type = "y", field = "dobre_zdravi", label = "Dobré zdraví", format = "percent", tab = "skutecnost")
    ),
    source = list(
        other_source("Eurostat", "Self-perceived health by sex, age and income quintile (hlth_silc_10)",
                     "https://ec.europa.eu/eurostat/databrowser/view/hlth_silc_10/default/table"),
        other_source("Eurostat", "Population on 1 January by age and sex (demo_pjan)",
                     "https://ec.europa.eu/eurostat/databrowser/view/demo_pjan/default/table")
    ),
    data_note = "Ekvivalizované příjmy podle počtu členů domácnosti. Starší lidé mají přirozeně nižší příjmy, a tak mají nižší kvintily horší zdraví částečně kvůli stáří osob. Čísla očištěná od věku jsou vážený průměr respondentů tak, aby měl každý kvintil stejnou věkovou strukturu." # "Podíl lidí ve věku 16 a více let, kteří své zdraví hodnotí jako dobré nebo velmi dobré, podle pětiny (kvintilu) ekvivalizovaného příjmu domácnosti. Šetření EU-SILC (Životní podmínky). V nižších kvintilech je víc starších lidí. Údaje očištěné o věk ukazují, jaký by byl podíl v každém kvintilu, kdyby měl stejnou věkovou strukturu jako všichni obyvatelé ČR ve věku 16+ (přímá standardizace v sedmi věkových skupinách 16–24 až 75+)."
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
    title = "Kojenecká úmrtnost v ČR od roku 1919",
    subtitle = "Procento živě narozených dětí, které zemřelo do jednoho roku věku",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "umrtnost", label = "Kojenecká úmrtnost", format = "percent")
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
    title = "Mateřská úmrtnost v ČR od roku 1985",
    subtitle = "Počet žen zemřelých v souvislosti těhotenství, porodu či šestinedělí na 100 000 živě narozených dětí, odhad",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "umrtnost", label = "Mateřská úmrtnost")
    ),
    source = other_source("WHO", "Maternal mortality ratio (per 100 000 live births), UN MMEIG estimates",
                          "https://www.who.int/data/gho/data/indicators/indicator-details/GHO/maternal-mortality-ratio-(per-100-000-live-births)"),
    data_note = "Modelový odhad -- případy jsou v dnes v ČR relativně vzácné a skutečné údaje by vykazovaly značný statistický šum." # "Mateřská úmrtnost: počet žen zemřelých v souvislosti s těhotenstvím, porodem nebo do 42 dnů po něm na 100 000 živě narozených dětí. Modelové odhady WHO a OSN (UN MMEIG); případů je v Česku několik ročně, proto jde o odhad s nejistotou."
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
    title = "Příčiny úmrtí v ČR od roku 2000",
    subtitle = "Poměr úmrtí podle vykázané příčiny, vlastní kategorizace",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "podil", label = "Podíl", format = "percent"),
        list(type = "colour", field = "pricina", label = "Příčina smrti")
    ),
    source = csu_source("Zemřelí podle pohlaví, pětiletých věkových skupin a zkráceného seznamu příčin smrti", "OBY04CT01"),
    options = list(facet = TRUE),
    data_note = "Kategorice příčin na základě kódů MKN-10: srdce a cévy I00–I99, nádory C00–D48, infekce dýchací soustavy: chřipka a záněty plic (J09–J18) a covid-19 (U07, U10), ostatní infekční A00–B99, vnější příčiny V01–Y98. Ostatní nepřenosné nemoci zahrnují i chronické nemoci dýchací soustavy (např. CHOPN) a nepřesně určené příčiny (R00–R99)."
)

# Počet zemřelých podle příčiny (skládaný sloupcový graf)
write_chart(
    df = skupiny |> transmute(year, pricina, zemreli = zemreli / 1e3),
    id = "zdravi-priciny-pocet",
    type = "bar",
    title = "Úmrtí v ČR od roku 2000",
    subtitle = "Počet úmrtí podle vykázané příčiny, vlastní kategorizace, tisíce",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "zemreli", label = "Úmrtí", unit = "tis."),
        list(type = "fill", field = "pricina", label = "Příčina smrti")
    ),
    source = csu_source("Zemřelí podle pohlaví, pětiletých věkových skupin a zkráceného seznamu příčin smrti", "OBY04CT01"),
    options = list(facet = TRUE),
    data_note = "Kategorice příčin na základě kódů MKN-10: srdce a cévy I00–I99, nádory C00–D48, infekce dýchací soustavy: chřipka a záněty plic (J09–J18) a covid-19 (U07, U10), ostatní infekční A00–B99, vnější příčiny V01–Y98. Ostatní nepřenosné nemoci zahrnují i chronické nemoci dýchací soustavy (např. CHOPN) a nepřesně určené příčiny (R00–R99)."
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
    title = "Rizikové faktory v ČR",
    subtitle = "Odhadovaný poměr kuřáků a dospělých s nadváhou a obezitou",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "podil", label = "Poměr obyvatel", format = "percent"),
        list(type = "colour", field = "ukazatel", label = "Rizikový faktor")
    ),
    source = list(
        other_source("WHO", "Prevalence of overweight among adults, BMI ≥ 25, age-standardized (NCD-RisC)",
                     "https://www.who.int/data/gho/data/indicators/indicator-details/GHO/prevalence-of-overweight-among-adults-bmi-=-25-(age-standardized-estimate)-(-)"),
        other_source("WHO", "Prevalence of obesity among adults, BMI ≥ 30, age-standardized (NCD-RisC)",
                     "https://www.who.int/data/gho/data/indicators/indicator-details/GHO/prevalence-of-obesity-among-adults-bmi-=-30-(age-standardized-estimate)-(-)"),
        other_source("WHO", "Estimate of current tobacco smoking prevalence, age-standardized",
                     "https://www.who.int/data/gho/data/indicators/indicator-details/GHO/gho-tobacco-control-monitor-current-tobaccosmoking-prevalence-ages-15plus-age-standardized")
    ),
    data_note = "Modelové odhady WHO, v standardizované podle věku pro mezinárodní srovnatelnost."
)

# ---------- (8b) Znečištění ovzduší (PM2.5) ----------
ovzdusi <- read_csv("./data/owid_pm25_air_pollution.csv") |>
    filter(Code == "CZE") |>
    transmute(year = Year, pm25 = `Outdoor air pollution exposure (population-weighted PM2.5)`) |>
    arrange(year)

write_chart(
    df = ovzdusi,
    id = "zdravi-ovzdusi",
    type = "line",
    title = "Znečištění vzduchu v ČR od roku 1998",
    subtitle = "Koncentrace škodlivých prachových částic (PM2.5) ve vzduchu, satelitní odhad",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "pm25", label = "Koncentrace PM2.5", unit = "µg/m³")
    ),
    options = list(annotations = list(list(y = 5, label = "Doporučení WHO"))), # label: "Doporučení WHO (5 µg/m³)"
    source = other_source("SatPM, Washington University in St. Louis", "Outdoor air pollution exposure, population-weighted PM2.5 (via Our World in Data)",
                          "https://ourworldindata.org/grapher/pm25-air-pollution"),
    data_note = "Průměrná roční koncentrace částic PM2.5, prostorové odhady na území ČR vážené podle hustoty obyvatel."
)

# ---------- (8c) Úmrtí a ztracené roky zdravého života (DALY) podle rizikových faktorů (GBD 2023) ----------
# Staženo ručně z GBD Results tool (https://vizhub.healthdata.org/gbd-results/): Česko, obě pohlaví, všechny
# věky, všechny příčiny, rizikové faktory 2. úrovně, rok 2023, Deaths a DALYs (Number, Percent, Rate).
rizika <- c(
    "High systolic blood pressure" = "Vysoký krevní tlak",
    "Dietary risks" = "Nevhodná strava",
    "Tobacco" = "Tabák",
    "High fasting plasma glucose" = "Vysoká hladina cukru v krvi",
    "High body-mass index" = "Nadváha a obezita",
    "High LDL cholesterol" = "Vysoký LDL cholesterol",
    "Other environmental risks" = "Olovo a radon",
    "Air pollution" = "Znečištění ovzduší",
    "Kidney dysfunction" = "Snížená funkce ledvin",
    "High alcohol use" = "Alkohol",
    "Non-optimal temperature" = "Horko a chlad",
    "Low physical activity" = "Nedostatek pohybu",
    "Occupational risks" = "Pracovní rizika",
    "Low bone mineral density" = "Řídnutí kostí",
    "Drug use" = "Drogy",
    "Unsafe sex" = "Nechráněný sex",
    "Sexual violence against children and bullying" = "Sexuální násilí na dětech a šikana",
    "Child and maternal malnutrition" = "Podvýživa matek a dětí",
    "Unsafe water, sanitation, and handwashing" = "Nebezpečná voda a nedostatečná hygiena",
    "Intimate partner violence" = "Násilí v partnerském vztahu"
)
# Skupiny rizik podle GBD (1. úroveň hierarchie)
skupiny_rizik <- c(
    "Environmentální a pracovní rizika" = c("Air pollution", "Other environmental risks", "Non-optimal temperature",
                                            "Occupational risks", "Unsafe water, sanitation, and handwashing"),
    "Behaviorální rizika" = c("Dietary risks", "Tobacco", "High alcohol use", "Low physical activity", "Drug use",
                              "Unsafe sex", "Sexual violence against children and bullying",
                              "Child and maternal malnutrition", "Intimate partner violence"),
    "Metabolická rizika" = c("High systolic blood pressure", "High fasting plasma glucose", "High body-mass index",
                             "High LDL cholesterol", "Kidney dysfunction", "Low bone mineral density")
)
skupina_rizika <- set_names(str_remove(names(skupiny_rizik), "\\d+$"), skupiny_rizik)

gbd_zip <- "./data/IHME-GBD_2023_DATA-1bb22f58-1.zip"
gbd <- read_csv(unz(gbd_zip, "IHME-GBD_2023_DATA-1bb22f58-1.csv")) |>
    filter(metric == "Number") |>
    transmute(year, riziko = rizika[rei], skupina = skupina_rizika[rei], measure = if_else(measure == "Deaths", "umrti", "daly"), val) |>
    pivot_wider(names_from = measure, values_from = val) |>
    mutate(skupina = factor(skupina, names(skupiny_rizik) |> str_remove("\\d+$") |> unique())) |>
    arrange(skupina, desc(umrti)) # pořadí v legendě; sloupce řadí `sort`
stopifnot(!anyNA(gbd$riziko), !anyNA(gbd$skupina))

write_chart(
    df = gbd,
    id = "zdravi-rizika-gbd",
    type = "bar",
    title = "Rizikové faktory v ČR",
    subtitle = str_glue("Počet úmrtí a ztracených [let zdravého života::DALY (Disability adjusted life years); Pokud se např. očekává, že daný člověk v průměru zemře v 80 letech, ale zemřel či se stal těžce postiženým v 55 letech, ztratil 25 let zdravého života] v roce {max(gbd$year)} z důvodu rizikových faktorů. Roky ve zdraví zdůrazňují faktory, které zabíjí v mladším věku. Odhad"),
    tabs = list(list(id = "umrti", label = "Úmrtí"), list(id = "daly", label = "Ztracené roky zdravého života")), # "Úmrtí", "Ztracené roky zdravého života (DALY)"
    aes = list(
        list(type = "x", field = "riziko", label = "Rizikový faktor"),
        list(type = "y", field = "umrti", label = "Počet úmrtí", tab = "umrti"),
        list(type = "y", field = "daly", label = "Ztracené roky ve zdraví", tab = "daly"),
        list(type = "fill", field = "skupina", label = "Druh rizikového faktoru")
    ),
    options = list(horizontal = TRUE, sort = "desc"),
    source = other_source("IHME", "Global Burden of Disease Study 2023 (GBD 2023) Results", "https://vizhub.healthdata.org/gbd-results/"),
    data_note = "Rizikové faktory se překrývají -- např. obezita je často spojená s vysokým krevním tlakem. " # "Podíl všech úmrtí a všech ztracených let zdravého života (DALY) v ČR v roce 2023, které lze podle modelu Global Burden of Disease přičíst danému rizikovému faktoru. DALY = roky ztracené předčasnou smrtí a roky prožité s nemocí nebo postižením. Rizika se překrývají (např. obezita zvyšuje krevní tlak), podíly proto nelze sčítat."
)

# ---------- (9) Pacienti ambulantní psychiatrie podle základní diagnózy ----------
diagnozy <- c(
    "F32–F33" = "Poruchy nálady (vč. deprese)", "F30-F39 bez F32&F33" = "Poruchy nálady (vč. deprese)",
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
    title = "Výskyt duševních poruch v ČR",
    subtitle = "Počet pacientů v [ambulantní::docházející za doktorem, tedy ne hospitalizovaní] péči psychiatra podle diagnózy, vlastní kategorizace",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "pacienti", label = "Počet pacientů", unit = "tis."),
        list(type = "colour", field = "diagnoza", label = "Diagnóza")
    ),
    source = other_source("ÚZIS", "Ambulantní péče v psychiatrii (NRHZS)",
                          "https://www.nzip.cz/data/2058-ambulantni-pece-psychiatrie-otevrena-data"),
    options = list(facet = TRUE),
    data_note = "Počet pacientů ambulantní psychiatrické péče hrazené z veřejného zdravotního pojištění podle základní diagnózy (MKN-10): Poruchy nálady F30–F39, úzkostné poruchy F40–F49, demence F00–F03 a Alzheimerova nemoc G30, závislosti F10–F19, poruchy s nástupem v dětství a dospívání F70–F99 (mentální retardace, poruchy psychického vývoje včetně autismu, ADHD a poruchy chování a emocí se začátkem v dětství a dospívání). Ostatní: hlavně schizofrenie, bipolární porucha, poruchy příjmu potravy a osobnosti."
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
    title = "Míra sebevražd v ČR od roku 1986",
    subtitle = "Počet sebevražd na 100 000 obyvatel",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "sebevrazdy", label = "Sebevražd na 100 000 obyvatel")
    ),
    source = other_source("WHO", "Age-standardized suicide rate per 100 000 (WHO Mortality Database, via Our World in Data)",
                          "https://ourworldindata.org/grapher/suicide-rate-who-mdb"),
    data_note = "Pro mezinárodní srovnatelnost standardizováno podle věku"
)
