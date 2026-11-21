library(tidyverse)
source("./R/utils.R")

# Veřejné finance: daně
# Viditelné texty (title, subtitle, label) jsou zatím prázdné; návrhy jsou v komentářích.

other_source <- function(name, dataset, url) list(name = name, dataset = dataset, url = url, retrieved = format(Sys.Date()))
oecd_source <- other_source("OECD", "Global Revenue Statistics Database (DF_RSGLOBAL)",
                            "https://www.oecd.org/en/data/datasets/global-revenue-statistics-database.html")

# Druhy daní podle klasifikace OECD; DPH = všeobecné daně ze zboží a služeb (5110: DPH, v USA daň z obratu); pojistné (sociální a zdravotní) a ostatní daně, aby součet dal všechny daně
dane <- c(T_1100 = "Daň z příjmů fyzických osob", T_1200 = "Daň z příjmů právnických osob", T_4000 = "Majetkové daně",
          T_5110 = "DPH", T_5121 = "Spotřební daně", T_2000 = "Sociální a zdravotní pojištění")
poradi <- c(dane[c("T_1100", "T_2000", "T_5110", "T_5121", "T_4000", "T_1200")], "Ostatní daně")

oecd <- read_csv("./data/oecd_revenue_statistics.csv")
# Ostatní = celkem minus vyjmenované druhy
po_druzich <- function(df) {
    df |>
        pivot_wider(names_from = dan, values_from = hodnota) |>
        mutate(T_OST = `_T` - T_1100 - T_1200 - T_4000 - T_5110 - T_5121 - T_2000) |>
        select(-`_T`) |>
        pivot_longer(c(starts_with("T_")), names_to = "dan", values_to = "hodnota") |>
        mutate(dan = factor(c(dane, T_OST = "Ostatní daně")[dan], poradi))
}

# ---------- (1) Výběr daní v ČR podle druhu ----------
# Inflace ČSÚ (od 1997), dřívější roky ze Světové banky (stejný index spotřebitelských cen, v překryvu se shoduje)
inflace_czso <- read_csv("./data/czso_CEN0101H.csv") |>
    filter(Ukazatel == "Průměrná roční míra inflace", !str_detect(CASRMX, "-")) |>
    transmute(year = as.integer(CASRMX), inflace = Hodnota)
inflace_wb <- read_csv("./data/wdi_inflace_cz.csv") |>
    filter(!is.na(inflace), year < min(inflace_czso$year)) |>
    transmute(year = as.integer(year), inflace)
ceny <- bind_rows(inflace_wb, inflace_czso) |>
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
stopifnot(!anyNA(vyber))

write_chart(
    df = vyber,
    id = "dane-vyber",
    type = "bar",
    title = "Daňové příjmy ČR", # "Kolik státu přinášejí jednotlivé daně"
    subtitle = "Daňové příjmy veřejného sektoru podle druhu daně", # "Daňové příjmy vládního sektoru v ČR podle druhu daně"
    tabs = list(list(id = "nominalni", label = "Nominální"), list(id = "realne", label = "Reálné"), list(id = "podil", label = "% daní")),
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "mld", label = "Nominální příjmy", unit = "mld. Kč", tab = "nominalni"),
        list(type = "y", field = "mld_2020", label = "Reálné příjmy", unit = "mld. Kč", tab = "realne"),
        list(type = "y", field = "podil", label = "% daňových příjmů", format = "percent", tab = "podil"),
        list(type = "fill", field = "dan", label = "Daň")
    ),
    source = list(oecd_source, other_source("ČSÚ", "Míra inflace (CEN0101H)", "https://data.csu.gov.cz/datastat/data/VYBER/CEN0101HT01"),
                  other_source("Světová banka", "Inflation, consumer prices (FP.CPI.TOTL.ZG), roky 1993–1996",
                               "https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG?locations=CZ")),
    options = list(facet = TRUE),
    data_note = "Daně vybrané v celém veřejném sektoru, i ty proudící mimo státní rozpočet. Reálné částky jsou přepočteny na ceny v roce 2020 (ČSÚ, před rokem 1997 Světová banka)." # "Daně vybrané všemi úrovněmi vlády (stát, kraje, obce, zdravotní pojišťovny) podle metodiky OECD. Sociální a zdravotní pojištění zahrnuje pojistné zaměstnanců, zaměstnavatelů a OSVČ. Ostatní daně: hlavně silniční daň, cla, poplatky a daně z hazardu. Ceny roku 2020 podle průměrné roční míry inflace (ČSÚ, před rokem 1997 Světová banka)."
)

# ---------- (2) Mezinárodní srovnání: skladba daní, % všech daňových příjmů ----------
rok_dane <- 2023 # poslední rok s průměrem OECD
zeme <- c(CZE = "Česko", DEU = "Německo", AUT = "Rakousko", SVK = "Slovensko", POL = "Polsko", FRA = "Francie",
          GBR = "Spojené království", SWE = "Švédsko", DNK = "Dánsko", FIN = "Finsko", NOR = "Norsko", USA = "USA",
          EST = "Estonsko", # nejnižší podíl majetkových daní v OECD (2023), těsně pod Českem
          OECD_REP = "Průměr OECD")
# podíl na všech daních (PT_OTR_SECTOR) a na HDP (PT_B1GQ): druhý ukazuje i to, kolik země vybírá celkem
skladba <- oecd |>
    filter(jednotka %in% c("PT_OTR_SECTOR", "PT_B1GQ"), rok == rok_dane) |>
    pivot_wider(names_from = jednotka, values_from = hodnota) |>
    pivot_longer(c(PT_OTR_SECTOR, PT_B1GQ), names_to = "jednotka", values_to = "hodnota") |>
    nest(.by = jednotka) |>
    mutate(data = map(data, po_druzich)) |>
    unnest(data) |>
    pivot_wider(names_from = jednotka, values_from = hodnota) |>
    transmute(year = rok, zeme = zeme[kod], dan,
              podil = round(PT_OTR_SECTOR / 100, 4), hdp = round(PT_B1GQ / 100, 4), cesko = kod == "CZE") |>
    # země seřazené podle podílu daně z příjmů fyzických osob na všech daních, nejvyšší nahoře
    mutate(zeme = fct_reorder(zeme, if_else(dan == dane[["T_1100"]], -podil, NA), .fun = \(v) min(v, na.rm = TRUE))) |>
    arrange(zeme, dan)
stopifnot(n_distinct(skladba$zeme) == length(zeme), !anyNA(skladba))

write_chart(
    df = skladba,
    id = "dane-skladba-srovnani",
    type = "bar",
    title = "Srovnání skladby daní", # str_glue("Z jakých daní žijí státy, rok {rok_dane}")
    subtitle = str_glue("Daňové příjmy podle druhu daně v roce {max(skladba$year)}"), # "Podíl jednotlivých druhů daní na všech daňových příjmech vládního sektoru"
    aes = list(
        list(type = "x", field = "zeme", label = "Země"),
        list(type = "y", field = "podil", label = "% daňových příjmů", format = "percent", tab = "podil"),
        list(type = "y", field = "hdp", label = "% HDP", format = "percent", tab = "hdp"),
        list(type = "fill", field = "dan", label = "Daň"),
        list(type = "highlight", field = "cesko")
    ),
    tabs = list(list(id = "podil", label = "% daňových příjmů"), list(id = "hdp", label = "% HDP")),
    source = oecd_source,
    options = list(horizontal = TRUE, facet = TRUE),
    data_note = "Daně vybrané v celém veřejném sektoru, i ty proudící mimo státní rozpočet. Průměr OECD: nevážený průměr členských zemí. DPH zahrnuje i ostatní všeobecné daně ze zboží a služeb, hlavně daň z obratu (sales tax) v USA, která DPH nahrazuje" # str_glue("Rok {rok_dane}. Daně a pojistné vybrané všemi úrovněmi vlády podle metodiky OECD. Průměr OECD je nevážený průměr členských zemí. Ve skandinávských zemích se sociální systém financuje hlavně z daně z příjmů, proto mají nízký podíl pojistného; v Dánsku pojistné téměř neexistuje.")
)
