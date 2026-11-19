library(tidyverse)
source("./R/utils.R")

# Veřejné finance: státní rozpočet
# Viditelné texty (title, subtitle, label) jsou zatím prázdné; návrhy jsou v komentářích.

eurostat_source <- function(id, dataset) {
    list(name = "Eurostat", dataset = str_glue("{dataset} ({id})"),
         url = str_glue("https://ec.europa.eu/eurostat/databrowser/view/{id}/default/table"),
         retrieved = format(Sys.Date()))
}
other_source <- function(name, dataset, url) list(name = name, dataset = dataset, url = url, retrieved = format(Sys.Date()))
monitor_source <- other_source("Monitor státní pokladny (MF ČR)", "Plnění rozpočtu ústředně řízených organizací, kapitoly státního rozpočtu",
                               "https://monitor.statnipokladna.gov.cz/")
imf_source <- other_source("MMF", "Fiscal Monitor a World Economic Outlook (DataMapper)", "https://www.imf.org/external/datamapper/")

imf <- read_csv("./data/imf_datamapper.csv")
rok_srovnani <- 2025 # poslední rok se skutečnými údaji Eurostatu

# Země ve srovnání: sousedé, Francie, Spojené království, severské země, EU, USA a svět.
# Evropské země a EU z Eurostatu, ostatní z MMF.
zeme_eurostat <- c(CZ = "Česko", DE = "Německo", AT = "Rakousko", SK = "Slovensko", PL = "Polsko", FR = "Francie",
                   SE = "Švédsko", DK = "Dánsko", FI = "Finsko", NO = "Norsko", EU27_2020 = "EU")
zeme_imf <- c(GBR = "Spojené království", USA = "USA", WEOWORLD = "Svět")

# ---------- (1) Velikost vládního sektoru: výdaje a příjmy, % HDP ----------
vladni <- read_csv("./data/eurostat_gov_10a_main.csv") |> filter(!is.na(values))
velikost <- vladni |>
    # bez roku 1995: první rok řady s výkyvem výdajů na 52,7 % HDP
    filter(geo == "CZ", time > 1995) |>
    transmute(year = time, ukazatel = if_else(na_item == "TE", "Výdaje", "Příjmy"), podil = values / 100) |>
    mutate(ukazatel = fct_relevel(ukazatel, "Výdaje")) |>
    arrange(ukazatel, year)

write_chart(
    df = velikost,
    id = "rozpocet-velikost",
    type = "line",
    title = "", # "Výdaje a příjmy veřejného sektoru v ČR od roku 1995"
    subtitle = "", # "Výdaje a příjmy vládního sektoru (stát, kraje a obce, zdravotní pojišťovny) jako podíl na HDP"
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "podil", label = "", format = "percent"), # "Podíl na HDP"
        list(type = "colour", field = "ukazatel", label = "") # "Ukazatel"
    ),
    source = eurostat_source("gov_10a_main", "Government revenue, expenditure and main aggregates"),
    options = list(y_zero = FALSE),
    data_note = "" # "Sektor vládních institucí podle metodiky ESA 2010: ústřední vláda včetně státních fondů, kraje a obce a zdravotní pojišťovny. Řada začíná rokem 1996."
)

# ---------- (2) Srovnání velikosti vládního sektoru: výdaje, % HDP ----------
# Svět: MMF průměr nepočítá, proto průměr zemí vážený HDP v USD
svet_vydaje <- imf |>
    filter(rok == rok_srovnani, ukazatel %in% c("G_X_G01_GDP_PT", "NGDPD")) |>
    pivot_wider(names_from = ukazatel, values_from = hodnota) |>
    filter(!is.na(G_X_G01_GDP_PT), !is.na(NGDPD)) |>
    summarise(vydaje = round(weighted.mean(G_X_G01_GDP_PT, NGDPD) / 100, 3))

srovnani_vydaje <- bind_rows(
    vladni |> filter(na_item == "TE", time == rok_srovnani, geo %in% names(zeme_eurostat)) |>
        transmute(zeme = zeme_eurostat[geo], vydaje = values / 100),
    imf |> filter(ukazatel == "G_X_G01_GDP_PT", rok == rok_srovnani, kod %in% c("GBR", "USA")) |>
        transmute(zeme = zeme_imf[kod], vydaje = round(hodnota / 100, 3)),
    mutate(svet_vydaje, zeme = "Svět")
) |>
    mutate(cesko = zeme == "Česko")
stopifnot(nrow(srovnani_vydaje) == length(zeme_eurostat) + length(zeme_imf), !anyNA(srovnani_vydaje))

write_chart(
    df = srovnani_vydaje,
    id = "rozpocet-velikost-srovnani",
    type = "bar",
    title = "", # str_glue("Velikost veřejného sektoru ve srovnání se světem v roce {rok_srovnani}")
    subtitle = "", # "Výdaje vládního sektoru jako podíl na HDP"
    aes = list(
        list(type = "x", field = "zeme", label = ""), # "Země"
        list(type = "y", field = "vydaje", label = "", format = "percent"), # "Výdaje (% HDP)"
        list(type = "highlight", field = "cesko")
    ),
    source = list(eurostat_source("gov_10a_main", "Government revenue, expenditure and main aggregates"), imf_source),
    options = list(horizontal = TRUE, sort = "desc"),
    data_note = "" # str_glue("Celkové výdaje vládního sektoru v roce {rok_srovnani}. Evropské země a EU: Eurostat; Spojené království a USA: MMF (Fiscal Monitor, část hodnot jsou odhady MMF). Svět: průměr zemí z Fiscal Monitoru vážený HDP v amerických dolarech.")
)

# ---------- Státní rozpočet podle Monitoru (3–5) ----------
monitor <- read_csv("./data/monitor_sr.csv") |>
    mutate(mld = mil_kc / 1e3, druh = case_when(seskupeni < 50 ~ "prijmy", seskupeni < 80 ~ "vydaje", .default = "financovani"))

# Přepočet na ceny roku 2020 podle průměrné roční míry inflace (ČSÚ), jako u reálných mezd
ceny <- read_csv("./data/czso_CEN0101H.csv") |>
    filter(Ukazatel == "Průměrná roční míra inflace", !str_detect(CASRMX, "-")) |>
    transmute(year = as.integer(CASRMX), inflace = Hodnota) |>
    arrange(year) |>
    mutate(ceny_2020 = cumprod(1 + inflace / 100),
           ceny_2020 = ceny_2020 / ceny_2020[year == 2020])
realne <- function(df) {
    out <- left_join(df, select(ceny, year, ceny_2020), by = "year") |>
        mutate(mld_2020 = round(mld / ceny_2020, 1), .after = mld) |> select(-ceny_2020)
    stopifnot("Chybí inflace za některý rok" = !anyNA(out$mld_2020))
    out
}
tabs_ceny <- list(list(id = "nominalni", label = ""), list(id = "realne", label = "")) # "Běžné ceny", "Ceny roku 2020"
inflace_source <- other_source("ČSÚ", "Míra inflace (CEN0101H)", "https://data.csu.gov.cz/datastat/data/VYBER/CEN0101HT01")

# (3) Příjmy a výdaje: výdaje nad osou, pod osou příjmy a schodek (kolik si stát půjčil), takže obě strany jsou stejně
# velké. V přebytkovém roce je přebytek nad osou s výdaji.
saldo <- monitor |>
    filter(druh != "financovani") |>
    summarise(mld = sum(mld), .by = c(rok, druh)) |>
    pivot_wider(names_from = druh, values_from = mld) |>
    mutate(saldo = prijmy - vydaje)
prijmy_vydaje <- bind_rows(
    transmute(saldo, year = rok, polozka = "Výdaje", mld = vydaje),
    transmute(saldo, year = rok, polozka = "Příjmy", mld = -prijmy),
    transmute(saldo, year = rok, polozka = "Schodek", mld = pmin(saldo, 0)),
    transmute(saldo, year = rok, polozka = "Přebytek", mld = pmax(saldo, 0))
) |>
    filter(mld != 0) |>
    mutate(polozka = fct_inorder(polozka), mld = round(mld, 1)) |>
    arrange(year, polozka) |>
    realne()

write_chart(
    df = prijmy_vydaje,
    id = "rozpocet-prijmy-vydaje",
    type = "bar",
    title = "", # "Příjmy a výdaje státního rozpočtu od roku 2010"
    subtitle = "", # "Výdaje (nahoře) jsou kryté příjmy a schodkem, tedy novým dluhem (dole), v miliardách Kč"
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "mld", label = "", unit = "mld. Kč", tab = "nominalni"), # "Částka"
        list(type = "y", field = "mld_2020", label = "", unit = "mld. Kč", tab = "realne"), # "Částka v cenách roku 2020"
        list(type = "fill", field = "polozka", label = "") # "Položka"
    ),
    source = list(monitor_source, inflace_source),
    tabs = tabs_ceny,
    data_note = "" # "Skutečné příjmy a výdaje kapitol státního rozpočtu (bez státních fondů, krajů, obcí a zdravotních pojišťoven), v běžných cenách nebo přepočtené na ceny roku 2020 podle míry inflace. Příjmy a schodek jsou pod osou, aby bylo vidět, že dohromady pokrývají výdaje."
)

# (4) Hrubé kategorie příjmů podle druhového třídění: daně (seskupení 11–15, 17), pojistné (16), přijaté transfery (41–42,
# hlavně peníze z EU), ostatní (nedaňové a kapitálové příjmy, třídy 2 a 3)
kategorie_prijmu <- monitor |>
    filter(druh == "prijmy") |>
    mutate(kategorie = case_when(
        seskupeni == 16 ~ "Pojistné",
        seskupeni < 20 ~ "Daně",
        seskupeni >= 40 ~ "Přijaté transfery",
        .default = "Ostatní"
    )) |>
    summarise(mld = round(sum(mld), 1), .by = c(rok, kategorie)) |>
    transmute(year = rok, kategorie = factor(kategorie, c("Daně", "Pojistné", "Přijaté transfery", "Ostatní")), mld) |>
    arrange(year, kategorie) |>
    realne()

write_chart(
    df = kategorie_prijmu,
    id = "rozpocet-prijmy",
    type = "bar",
    title = "", # "Z čeho stát žije: příjmy státního rozpočtu"
    subtitle = "", # "Příjmy státního rozpočtu podle druhu, v miliardách Kč"
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "mld", label = "", unit = "mld. Kč", tab = "nominalni"), # "Příjmy"
        list(type = "y", field = "mld_2020", label = "", unit = "mld. Kč", tab = "realne"), # "Příjmy v cenách roku 2020"
        list(type = "fill", field = "kategorie", label = "") # "Druh příjmu"
    ),
    source = list(monitor_source, inflace_source),
    tabs = tabs_ceny,
    data_note = "" # "V běžných cenách nebo přepočtené na ceny roku 2020 podle míry inflace. Druhové třídění rozpočtové skladby. Daně: daně z příjmů, DPH, spotřební, majetkové a další daně. Pojistné: sociální pojištění (důchodové, nemocenské a příspěvek na politiku zaměstnanosti). Přijaté transfery: hlavně peníze z rozpočtu EU. Ostatní: nedaňové a kapitálové příjmy (poplatky, dividendy, prodej majetku). Část daní (sdílené daně) jde krajům a obcím a v rozpočtu státu není."
)

# (5) Hrubé kategorie výdajů podle odvětvového třídění (skupiny paragrafů)
skupiny <- c(`1` = "Hospodářství a infrastruktura", `2` = "Hospodářství a infrastruktura", `3` = "Služby", `4` = "Důchody a sociální transfery",
             `5` = "Bezpečnost státu", `6` = "Veřejná správa")
kategorie_vydaju <- monitor |>
    filter(druh == "vydaje", skupina != 0) |>
    mutate(skupina = skupiny[as.character(skupina)]) |>
    summarise(mld = round(sum(mld), 1), .by = c(rok, skupina)) |>
    transmute(year = rok, skupina = factor(skupina, c("Důchody a sociální transfery", "Služby", "Hospodářství a infrastruktura",
                                                      "Bezpečnost státu", "Veřejná správa")), mld) |>
    arrange(year, skupina) |>
    realne()
stopifnot(!anyNA(kategorie_vydaju))

write_chart(
    df = kategorie_vydaju,
    id = "rozpocet-vydaje",
    type = "bar",
    title = "", # "Na co stát utrácí: výdaje státního rozpočtu"
    subtitle = "", # "Výdaje státního rozpočtu podle oblasti, v miliardách Kč"
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "mld", label = "", unit = "mld. Kč", tab = "nominalni"), # "Výdaje"
        list(type = "y", field = "mld_2020", label = "", unit = "mld. Kč", tab = "realne"), # "Výdaje v cenách roku 2020"
        list(type = "fill", field = "skupina", label = "") # "Oblast"
    ),
    source = list(monitor_source, inflace_source),
    tabs = tabs_ceny,
    data_note = "" # "V běžných cenách nebo přepočtené na ceny roku 2020 podle míry inflace. Odvětvové třídění rozpočtové skladby. Důchody a sociální transfery: důchody, dávky, podpora v nezaměstnanosti, politika zaměstnanosti. Služby: vzdělávání (včetně platů učitelů placených přes kraje), zdravotnictví (včetně plateb za státní pojištěnce), kultura, bydlení, životní prostředí. Hospodářství a infrastruktura: doprava, zemědělství, průmysl, energetika, výzkum. Bezpečnost: obrana, policie, hasiči, soudy, vězeňství. Veřejná správa: státní a místní správa, úroky ze státního dluhu, transfery obcím a krajům."
)

# ---------- (6) Dluh vládního sektoru: Kč a % HDP, s projekcí MMF ----------
dluh_eu <- read_csv("./data/eurostat_gov_10dd_edpt1.csv") |> filter(!is.na(values))
dluh_skutecnost <- dluh_eu |>
    filter(geo == "CZ") |>
    pivot_wider(names_from = unit, values_from = values) |>
    transmute(year = time, data = "Skutečnost", mld = MIO_NAC / 1e3, podil = PC_GDP / 100)
rok_dluh <- max(dluh_skutecnost$year)
dluh_projekce <- read_csv("./data/imf_weo_cze_dluh.csv") |>
    filter(rok > rok_dluh) |>
    pivot_wider(names_from = ukazatel, values_from = hodnota) |>
    transmute(year = rok, data = "Projekce MMF", mld = GGXWDG / 1e9, podil = GGXWDG_NGDP / 100)
dluh <- bind_rows(
    dluh_skutecnost,
    filter(dluh_skutecnost, year == rok_dluh) |> mutate(data = "Projekce MMF"), # navázání projekce na skutečnost
    dluh_projekce
) |>
    mutate(data = fct_inorder(data), mld = round(mld, 1)) |>
    arrange(data, year)

write_chart(
    df = dluh,
    id = "rozpocet-dluh",
    type = "line",
    title = "", # "Veřejný dluh ČR od roku 1995 s projekcí do roku 2031"
    subtitle = "", # "Hrubý dluh vládního sektoru v miliardách Kč a jako podíl na HDP"
    tabs = list(list(id = "podil", label = ""), list(id = "kc", label = "")), # "% HDP", "Kč"
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "podil", label = "", format = "percent", tab = "podil"), # "Dluh (% HDP)"
        list(type = "y", field = "mld", label = "", unit = "mld. Kč", tab = "kc"), # "Dluh"
        list(type = "linetype", field = "data", label = "") # "Údaj"
    ),
    source = list(eurostat_source("gov_10dd_edpt1", "Government deficit/surplus, debt and associated data"),
                  other_source("MMF", "World Economic Outlook, October 2026 (SDMX API)", "https://data.imf.org/")),
    data_note = "" # str_glue("Hrubý (maastrichtský) dluh vládního sektoru: stát, kraje a obce, zdravotní pojišťovny. Státní dluh v užším smyslu (dluh státního rozpočtu) je o něco nižší. Do roku {rok_dluh} Eurostat, poté projekce MMF (World Economic Outlook, říjen 2026).")
)

# ---------- (7) Srovnání dluhu, % HDP: stejné země jako (2) + nejvyšší a nejnižší dluh v OECD ----------
oecd <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL",
          "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN",
          "ESP", "SWE", "CHE", "TUR", "GBR", "USA")
nazvy_extremu <- c(JPN = "Japonsko", GRC = "Řecko", ITA = "Itálie", TUR = "Turecko", EST = "Estonsko", LUX = "Lucembursko")
imf_dluh <- filter(imf, ukazatel == "GGXWDG_NGDP")
extremy <- imf_dluh |>
    filter(rok == rok_srovnani, kod %in% oecd) |>
    filter(hodnota %in% range(hodnota)) |>
    pull(kod)
stopifnot("Nový extrém v OECD: doplň jeho název do nazvy_extremu" = all(extremy %in% names(nazvy_extremu)))
# Norsko v tabulce dluhu Eurostatu (EDP) není, bere se z MMF
zeme_eurostat_dluh <- zeme_eurostat[names(zeme_eurostat) != "NO"]
zeme_imf_dluh <- c(NOR = "Norsko", zeme_imf, nazvy_extremu[extremy])

dluh_zemi <- bind_rows(
    dluh_eu |> filter(unit == "PC_GDP", geo %in% names(zeme_eurostat_dluh)) |>
        transmute(zeme = zeme_eurostat_dluh[geo], rok = time, dluh = values),
    imf_dluh |> filter(kod %in% names(zeme_imf_dluh), rok <= rok_srovnani) |>
        transmute(zeme = zeme_imf_dluh[kod], rok, dluh = hodnota)
)
srovnani_dluh <- dluh_zemi |>
    filter(rok == rok_srovnani) |>
    transmute(zeme, dluh = round(dluh / 100, 3), cesko = zeme == "Česko")
stopifnot(nrow(srovnani_dluh) == length(zeme_eurostat_dluh) + length(zeme_imf_dluh))

write_chart(
    df = srovnani_dluh,
    id = "rozpocet-dluh-srovnani",
    type = "bar",
    title = "", # str_glue("Veřejný dluh ve srovnání se světem v roce {rok_srovnani}")
    subtitle = "", # "Hrubý dluh vládního sektoru jako podíl na HDP; Japonsko a Turecko mají nejvyšší a nejnižší dluh v OECD"
    aes = list(
        list(type = "x", field = "zeme", label = ""), # "Země"
        list(type = "y", field = "dluh", label = "", format = "percent"), # "Dluh (% HDP)"
        list(type = "highlight", field = "cesko")
    ),
    source = list(eurostat_source("gov_10dd_edpt1", "Government deficit/surplus, debt and associated data"), imf_source),
    options = list(horizontal = TRUE, sort = "desc"),
    data_note = "" # str_glue("Hrubý dluh vládního sektoru v roce {rok_srovnani}. Evropské země (bez Norska) a EU: Eurostat; ostatní: MMF (World Economic Outlook, část hodnot jsou odhady MMF; svět je průměr vážený HDP).")
)

# ---------- (8) Růst dluhu: změna v procentních bodech HDP za posledních 5 let dat ----------
rok_rust <- rok_srovnani - 5
rust_dluhu <- dluh_zemi |>
    filter(rok %in% c(rok_rust, rok_srovnani)) |>
    pivot_wider(names_from = rok, values_from = dluh, names_prefix = "r") |>
    transmute(zeme, zmena = round((.data[[str_c("r", rok_srovnani)]] - .data[[str_c("r", rok_rust)]]) / 100, 3),
              cesko = zeme == "Česko")
stopifnot(!anyNA(rust_dluhu))

write_chart(
    df = rust_dluhu,
    id = "rozpocet-dluh-rust",
    type = "bar",
    title = "", # "Jak rychle roste veřejný dluh"
    subtitle = "", # str_glue("Změna hrubého dluhu vládního sektoru v procentních bodech HDP mezi lety {rok_rust} a {rok_srovnani}")
    aes = list(
        list(type = "x", field = "zeme", label = ""), # "Země"
        list(type = "y", field = "zmena", label = "", format = "percent", unit = "p. b."), # "Změna dluhu"
        list(type = "highlight", field = "cesko")
    ),
    source = list(eurostat_source("gov_10dd_edpt1", "Government deficit/surplus, debt and associated data"), imf_source),
    options = list(horizontal = TRUE, sort = "desc"),
    data_note = "" # str_glue("Rozdíl mezi hrubým dluhem vládního sektoru (% HDP) v roce {rok_srovnani} a {rok_rust}. Evropské země (bez Norska) a EU: Eurostat; ostatní: MMF.")
)
