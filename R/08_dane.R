library(tidyverse)
source("./R/utils.R")

# Veřejné finance: daně
# Viditelné texty (title, subtitle, label) jsou zatím prázdné; návrhy jsou v komentářích.

other_source <- function(name, dataset, url) list(name = name, dataset = dataset, url = url, retrieved = format(Sys.Date()))
oecd_source <- other_source("OECD", "Global Revenue Statistics Database (DF_RSGLOBAL)",
                            "https://www.oecd.org/en/data/datasets/global-revenue-statistics-database.html")

# Druhy daní podle klasifikace OECD; pojistné (sociální a zdravotní) a ostatní daně, aby součet dal všechny daně
dane <- c(T_1100 = "Daň z příjmů fyzických osob", T_1200 = "Daň z příjmů právnických osob", T_4000 = "Majetkové daně",
          T_5111 = "DPH", T_5121 = "Spotřební daně", T_2000 = "Sociální a zdravotní pojištění")
poradi <- c(dane[c("T_1100", "T_2000", "T_5111", "T_5121", "T_4000", "T_1200")], "Ostatní daně")

oecd <- read_csv("./data/oecd_revenue_statistics.csv")
# Ostatní = celkem minus vyjmenované druhy
po_druzich <- function(df) {
    df |>
        pivot_wider(names_from = dan, values_from = hodnota) |>
        mutate(T_OST = `_T` - T_1100 - T_1200 - T_4000 - T_5111 - T_5121 - T_2000) |>
        select(-`_T`) |>
        pivot_longer(c(starts_with("T_")), names_to = "dan", values_to = "hodnota") |>
        mutate(dan = factor(c(dane, T_OST = "Ostatní daně")[dan], poradi))
}

# ---------- (1) Výběr daní v ČR podle druhu ----------
ceny <- read_csv("./data/czso_CEN0101H.csv") |>
    filter(Ukazatel == "Průměrná roční míra inflace", !str_detect(CASRMX, "-")) |>
    transmute(year = as.integer(CASRMX), inflace = Hodnota) |>
    arrange(year) |>
    mutate(ceny_2020 = cumprod(1 + inflace / 100),
           ceny_2020 = ceny_2020 / ceny_2020[year == 2020])

vyber <- oecd |>
    filter(kod == "CZE", jednotka %in% c("XDC", "PT_OTR_SECTOR")) |>
    pivot_wider(names_from = jednotka, values_from = hodnota) |>
    rename(mld = XDC, podil = PT_OTR_SECTOR) |>
    pivot_longer(c(mld, podil), names_to = "jednotka", values_to = "hodnota") |>
    group_by(jednotka) |>
    group_modify(\(d, k) po_druzich(d)) |>
    ungroup() |>
    pivot_wider(names_from = jednotka, values_from = hodnota) |>
    left_join(select(ceny, rok = year, ceny_2020), by = "rok") |>
    transmute(year = rok, dan, mld = round(mld, 1), mld_2020 = round(mld / ceny_2020, 1), podil = round(podil / 100, 4)) |>
    arrange(year, dan)
stopifnot(!anyNA(select(vyber, -mld_2020))) # ceny roku 2020 až od 1997 (inflace ČSÚ)

write_chart(
    df = vyber,
    id = "dane-vyber",
    type = "bar",
    title = "", # "Kolik státu přinášejí jednotlivé daně"
    subtitle = "", # "Daňové příjmy vládního sektoru v ČR podle druhu daně"
    tabs = list(list(id = "nominalni", label = ""), list(id = "realne", label = ""), list(id = "podil", label = "")), # "Běžné ceny", "Ceny roku 2020", "Podíl na daních"
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "mld", label = "", unit = "mld. Kč", tab = "nominalni"), # "Výběr"
        list(type = "y", field = "mld_2020", label = "", unit = "mld. Kč", tab = "realne"), # "Výběr v cenách roku 2020"
        list(type = "y", field = "podil", label = "", format = "percent", tab = "podil"), # "Podíl na všech daních"
        list(type = "fill", field = "dan", label = "") # "Daň"
    ),
    source = list(oecd_source, other_source("ČSÚ", "Míra inflace (CEN0101H)", "https://data.csu.gov.cz/datastat/data/VYBER/CEN0101HT01")),
    data_note = "" # "Daně vybrané všemi úrovněmi vlády (stát, kraje, obce, zdravotní pojišťovny) podle metodiky OECD. Sociální a zdravotní pojištění zahrnuje pojistné zaměstnanců, zaměstnavatelů a OSVČ. Ostatní daně: hlavně silniční daň, cla, poplatky a daně z hazardu. Ceny roku 2020 podle průměrné roční míry inflace (ČSÚ), od roku 1997."
)

# ---------- (2) Mezinárodní srovnání: skladba daní, % všech daňových příjmů ----------
rok_dane <- 2023 # poslední rok s průměrem OECD
zeme <- c(CZE = "Česko", DEU = "Německo", AUT = "Rakousko", SVK = "Slovensko", POL = "Polsko", FRA = "Francie",
          GBR = "Spojené království", SWE = "Švédsko", DNK = "Dánsko", FIN = "Finsko", NOR = "Norsko", USA = "USA",
          OECD_REP = "Průměr OECD")
skladba <- oecd |>
    filter(jednotka == "PT_OTR_SECTOR", rok == rok_dane) |>
    select(-jednotka) |>
    po_druzich() |>
    transmute(zeme = factor(zeme[kod], zeme), dan, podil = round(hodnota / 100, 4)) |>
    arrange(zeme, dan)
stopifnot(n_distinct(skladba$zeme) == length(zeme), !anyNA(skladba))

write_chart(
    df = skladba,
    id = "dane-skladba-srovnani",
    type = "bar",
    title = "", # str_glue("Z jakých daní žijí státy, rok {rok_dane}")
    subtitle = "", # "Podíl jednotlivých druhů daní na všech daňových příjmech vládního sektoru"
    aes = list(
        list(type = "x", field = "zeme", label = ""), # "Země"
        list(type = "y", field = "podil", label = "", format = "percent"), # "Podíl na daních"
        list(type = "fill", field = "dan", label = "") # "Daň"
    ),
    source = oecd_source,
    options = list(horizontal = TRUE),
    data_note = "" # str_glue("Rok {rok_dane}. Daně a pojistné vybrané všemi úrovněmi vlády podle metodiky OECD. Průměr OECD je nevážený průměr členských zemí. Ve skandinávských zemích se sociální systém financuje hlavně z daně z příjmů, proto mají nízký podíl pojistného; v Dánsku pojistné téměř neexistuje.")
)
