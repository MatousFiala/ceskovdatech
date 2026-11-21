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
    title = "Výdaje a příjmy veřejného sektoru v ČR od roku 1995", # "Výdaje a příjmy veřejného sektoru v ČR od roku 1995"
    subtitle = "Výdaje a příjmy státu, samospráv a zdravotních pojišťoven, % HDP", # "Výdaje a příjmy vládního sektoru (stát, kraje a obce, zdravotní pojišťovny) jako podíl na HDP"
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "podil", label = "% HDP", format = "percent"),
        list(type = "colour", field = "ukazatel", label = "Ukazatel")
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
    mutate(year = rok_srovnani, cesko = zeme == "Česko")
stopifnot(nrow(srovnani_vydaje) == length(zeme_eurostat) + length(zeme_imf), !anyNA(srovnani_vydaje))

write_chart(
    df = srovnani_vydaje,
    id = "rozpocet-velikost-srovnani",
    type = "bar",
    title = "Srovnání velikosti veřejného sektoru", # str_glue("Velikost veřejného sektoru ve srovnání se světem v roce {rok_srovnani}")
    subtitle = str_glue("Výdaje státu, samospráv a jiných státních organizací v roce {max(srovnani_vydaje$year)}"), # "Výdaje vládního sektoru jako podíl na HDP"
    aes = list(
        list(type = "x", field = "zeme", label = "Země"),
        list(type = "y", field = "vydaje", label = "Výdaje (% HDP)", format = "percent"),
        list(type = "highlight", field = "cesko")
    ),
    source = list(eurostat_source("gov_10a_main", "Government revenue, expenditure and main aggregates"), imf_source),
    options = list(horizontal = TRUE, sort = "desc"),
    data_note = "Evropské země a EU z dat Eurostatu, UK a USA z dat MMF. Svět: vážený průměr dat MMF podle HDP" # str_glue("Celkové výdaje vládního sektoru v roce {rok_srovnani}. Evropské země a EU: Eurostat; Spojené království a USA: MMF (Fiscal Monitor, část hodnot jsou odhady MMF). Svět: průměr zemí z Fiscal Monitoru vážený HDP v amerických dolarech.")
)

# ---------- Státní rozpočet podle Monitoru (3–5) ----------
monitor <- read_csv("./data/monitor_sr.csv", col_types = cols(pododdil = "c")) |>
    mutate(mld = mil_kc / 1e3, skupina = as.integer(str_sub(pododdil, 1, 1)), druh = case_when(seskupeni < 50 ~ "prijmy", seskupeni < 80 ~ "vydaje", .default = "financovani"))

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
tabs_ceny <- list(list(id = "nominalni", label = "Nominální"), list(id = "realne", label = "Reálné"))
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
    title = "Příjmy a výdaje státního ropočtu od roku 2018", # "Příjmy a výdaje státního rozpočtu od roku 2010"
    subtitle = "Výdaje a jejich krytí příjmy a schodkem, nominální i očištěné od inflace, miliardy Kč", # "Výdaje (nahoře) jsou kryté příjmy a schodkem, tedy novým dluhem (dole), v miliardách Kč"
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "mld", label = "Nominální částka", unit = "mld. Kč", tab = "nominalni"),
        list(type = "y", field = "mld_2020", label = "Reálná částka", unit = "mld. Kč", tab = "realne"),
        list(type = "fill", field = "polozka", label = "Položka")
    ),
    source = list(monitor_source, inflace_source),
    tabs = tabs_ceny,
    data_note = "Reálné částky jsou přepočtené na ceny v roce 2020. Příjmy a schodek jsou účetnicky pod osou -- z příjmových a dluhových účtů 'odchází' peníze pro financování výdajů" # "Skutečné příjmy a výdaje kapitol státního rozpočtu (bez státních fondů, krajů, obcí a zdravotních pojišťoven), v běžných cenách nebo přepočtené na ceny roku 2020 podle míry inflace. Příjmy a schodek jsou pod osou, aby bylo vidět, že dohromady pokrývají výdaje."
)

# (3b) Saldo státního rozpočtu (Monitor) v mld. Kč a v % nominálního HDP (Eurostat). Schodek je pod nulou.
hdp <- read_csv("./data/eurostat_gov_10dd_edpt1_cz.csv") |> transmute(year = time, B1GQ = values)
schodek <- transmute(saldo, year = rok, saldo_mld = round(saldo, 1)) |>
    left_join(hdp, by = "year") |>
    transmute(year, saldo_mld, saldo_hdp = round(saldo_mld * 1e3 / B1GQ, 4)) |>
    arrange(year)
stopifnot("Chybí HDP za některý rok Monitoru" = !anyNA(schodek))

write_chart(
    df = schodek,
    id = "rozpocet-schodek",
    type = "bar",
    title = "Schodek státního rozpočtu", # "Schodek státního rozpočtu"
    subtitle = "Rozdíl mezi příjmy a výdaji státního rozpočtu", # "Saldo státního rozpočtu v miliardách Kč a jako podíl na HDP"
    tabs = list(list(id = "nominalni", label = "Nominální"), list(id = "hdp", label = "% HDP")),
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "saldo_mld", label = "Schodek", unit = "mld. Kč", tab = "nominalni"),
        list(type = "y", field = "saldo_hdp", label = "Schodek (% HDP)", format = "percent", tab = "hdp")
    ),
    source = list(monitor_source, eurostat_source("gov_10dd_edpt1", "Government deficit/surplus, debt and associated data")),
    data_note = "Nominální schodek z dat MSP, podíl na HDP z dat Eurostatu" # "Saldo = příjmy − výdaje; schodek je pod nulou. Státní rozpočet: kapitoly státního rozpočtu podle Monitoru státní pokladny, od roku 2010; podíl na nominálním HDP (Eurostat)."
)

# (4) Hrubé kategorie příjmů podle druhového třídění: daně (seskupení 11–15, 17), pojistné (16), přijaté transfery (41–42,
# hlavně peníze z EU), ostatní (nedaňové a kapitálové příjmy, třídy 2 a 3)
kategorie_prijmu <- monitor |>
    filter(druh == "prijmy") |>
    mutate(kategorie = case_when(
        seskupeni == 16 ~ "Sociální pojištění",
        seskupeni < 20 ~ "Daně",
        seskupeni >= 40 ~ "Přijaté transfery",
        .default = "Ostatní"
    )) |>
    summarise(mld = round(sum(mld), 1), .by = c(rok, kategorie)) |>
    transmute(year = rok, kategorie = factor(kategorie, c("Daně", "Sociální pojištění", "Přijaté transfery", "Ostatní")), mld) |>
    arrange(year, kategorie) |>
    realne()

write_chart(
    df = kategorie_prijmu,
    id = "rozpocet-prijmy",
    type = "bar",
    title = "Příjmy státního rozpočtu od roku 2010", # "Z čeho stát žije: příjmy státního rozpočtu"
    subtitle = "Příjmy státního rozpočtu podle kategoriem, miliardy Kč", # "Příjmy státního rozpočtu podle druhu, v miliardách Kč"
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "mld", label = "Nominální příjem", unit = "mld. Kč", tab = "nominalni"),
        list(type = "y", field = "mld_2020", label = "Reálný příjem", unit = "mld. Kč", tab = "realne"),
        list(type = "fill", field = "kategorie", label = "Druh příjmu")
    ),
    source = list(monitor_source, inflace_source),
    tabs = tabs_ceny,
    options = list(facet = TRUE),
    data_note = "Reálné částky jsou přepočteny na ceny v roce 2020. Do sociálního pojištění se neřadí zdravotní odvody -- ty proudí státní rozpočet pojišťovnám. Přijaté transfery jsou převážně dotace EU." # "V běžných cenách nebo přepočtené na ceny roku 2020 podle míry inflace. Druhové třídění rozpočtové skladby. Daně: daně z příjmů, DPH, spotřební, majetkové a další daně. Sociální pojištění: důchodové, nemocenské a příspěvek na politiku zaměstnanosti; zdravotní pojištění jde zdravotním pojišťovnám, ne do státního rozpočtu. Přijaté transfery: hlavně peníze z rozpočtu EU. Ostatní: nedaňové a kapitálové příjmy (poplatky, dividendy, prodej majetku). Část daní (sdílené daně) jde krajům a obcím a v rozpočtu státu není."
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
    title = "Výdaje státního rozpočtu od roku 2010", # "Na co stát utrácí: výdaje státního rozpočtu"
    subtitle = "Výdaje státního rozpočtu podle kategorie, miliardy Kč", # "Výdaje státního rozpočtu podle oblasti, v miliardách Kč"
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "mld", label = "Nominální výdaje", unit = "mld. Kč", tab = "nominalni"),
        list(type = "y", field = "mld_2020", label = "Reálné výdaje", unit = "mld. Kč", tab = "realne"),
        list(type = "fill", field = "skupina", label = "Kategorie")
    ),
    source = list(monitor_source, inflace_source),
    tabs = tabs_ceny,
    options = list(facet = TRUE),
    data_note = "Reálné častky jsou přepočteny na ceny v roce 2020. Důchody a sociální transfery: veškeré důchody, sociální dávky a příspěvky. Služby: vzdělávání, zdravotnictví, kultura, bydlení, životní prostředí. Hospodářství: doprava, zemědělství, průmysl, energetika, výzkum. Bezpečnost: obrana, bezpečnostní složky. Veřejná správa: státní a místní správa, obsluha dluhu, transfery samosprávám." # "V běžných cenách nebo přepočtené na ceny roku 2020 podle míry inflace. Odvětvové třídění rozpočtové skladby. Důchody a sociální transfery: důchody, dávky, podpora v nezaměstnanosti, politika zaměstnanosti. Služby: vzdělávání (včetně platů učitelů placených přes kraje), zdravotnictví (včetně plateb za státní pojištěnce), kultura, bydlení, životní prostředí. Hospodářství a infrastruktura: doprava, zemědělství, průmysl, energetika, výzkum. Bezpečnost: obrana, policie, hasiči, soudy, vězeňství. Veřejná správa: státní a místní správa, úroky ze státního dluhu, transfery obcím a krajům."
)

# (5b) Podrobné výdaje v posledním roce: oddíly paragrafů (první dvě číslice), dávky (oddíl 41) po pododdílech;
# barva podle oblasti z (5)
oblasti_vydaju <- c(
    `10` = "Zemědělství a lesnictví", `21` = "Průmysl, obchod a služby", `22` = "Doprava",
    `23` = "Vodní hospodářství", `24` = "Spoje", `25` = "Ostatní hospodářství",
    `31` = "Vzdělávání", `32` = "Vzdělávání", `33` = "Kultura, církve a média", `34` = "Sport a zájmová činnost",
    `35` = "Zdravotnictví", `36` = "Bydlení a územní rozvoj", `37` = "Životní prostředí",
    `38` = "Výzkum a vývoj", `39` = "Ostatní služby pro obyvatele",
    `411` = "Důchody", `412` = "Nemocenské dávky", `413` = "Státní sociální podpora",
    `414` = "Státní sociální podpora", `415` = "Výsluhy vojáků a policistů",
    `416` = "Úrazové pojištění", `417` = "Pomoc v hmotné nouzi", `418` = "Dávky pro zdravotně postižené",
    `419` = "Příspěvek na péči",
    `42` = "Politika zaměstnanosti", `43` = "Sociální služby",
    `51` = "Obrana", `52` = "Krizová připravenost", `53` = "Policie a veřejný pořádek",
    `54` = "Soudy a vězeňství", `55` = "Hasiči a záchranný systém",
    `61` = "Státní správa a samospráva", `62` = "Jiné veřejné služby", `63` = "Úroky z dluhu a fin. operace",
    `64` = "Ostatní činnosti"
)
rok_vydaje <- max(monitor$rok)
vydaje_detail <- monitor |>
    filter(druh == "vydaje", rok == rok_vydaje, skupina != 0) |>
    mutate(kod = if_else(str_starts(pododdil, "41"), pododdil, str_sub(pododdil, 1, 2)),
           oblast = oblasti_vydaju[kod],
           skupina = factor(skupiny[as.character(skupina)], levels(kategorie_vydaju$skupina))) |>
    summarise(mld = round(sum(mld), 1), .by = c(oblast, skupina)) |>
    filter(mld > 0) |>
    mutate(podil = mld / sum(mld))
stopifnot("Oddíl bez názvu: doplň ho do oblasti_vydaju" = !anyNA(vydaje_detail))
# oblasti pod 0,5 % všech výdajů a ostatní činnosti veřejné správy jako jeden sloupec "Ostatní" (vždy dole),
# rozdělený barvou podle skupiny
ostatni_vydaje <- vydaje_detail |> filter(podil < 0.005 | oblast == "Ostatní činnosti") |> arrange(desc(mld)) |> pull(oblast)
vydaje_detail <- vydaje_detail |>
    mutate(oblast = if_else(oblast %in% ostatni_vydaje, "Ostatní", oblast)) |>
    summarise(mld = sum(mld), podil = sum(podil), .by = c(oblast, skupina)) |>
    mutate(year = rok_vydaje, mld = round(mld, 1), podil = round(podil, 4)) |>
    arrange(skupina, desc(mld)) # pořadí skupin = pořadí barev v (5); sloupce řadí options$sort

write_chart(
    df = vydaje_detail,
    id = "rozpocet-vydaje-detail",
    type = "bar",
    title = "Podrobné výdaje státního rozpočtu", # str_glue("Na co stát utratil v roce {rok_vydaje}")
    subtitle = str_glue("Výdaje státního rozpočtu podle oblasti v roce {max(vydaje_detail$year)}"), # "Výdaje státního rozpočtu podle oblasti, v miliardách Kč"
    aes = list(
        list(type = "x", field = "oblast", label = "Oblast"),
        list(type = "y", field = "mld", label = "Výdaje", unit = "mld. Kč", tab = "mld"),
        list(type = "y", field = "podil", label = "% výdajů", format = "percent", tab = "podil"),
        list(type = "fill", field = "skupina", label = "Kategorie")
    ),
    tabs = list(list(id = "mld", label = "Nominální"), list(id = "podil", label = "% výdajů")),
    source = monitor_source,
    options = list(horizontal = TRUE, sort = "desc", sort_last = "Ostatní", facet = TRUE, facet_columns = 2),
    data_note = str_glue("Ostatní: ostatní činnosti veřejné správy a oblasti, které mají každá méně než 0,5 % výdajů ({str_flatten_comma(str_to_lower(setdiff(ostatni_vydaje, 'Ostatní činnosti')))}). Vzdělávání zahrnuje i platy učitelů v regionálním školství, které stát posílá přes kraje. Zdravotnictví zahrnuje i platbu státu do zdravotního pojištění za státní pojištěnce (děti, studenty, důchodce a další). Příspěvek na péči zahrnuje i státní příspěvky na penzijní spoření a další drobné dávky sociálního zabezpečení.") # str_glue("Výdaje kapitol státního rozpočtu v roce {rok_vydaje} podle oddílů odvětvového třídění rozpočtové skladby; sociální dávky podle pododdílů. Vzdělávání zahrnuje platy učitelů, které stát posílá přes kraje. Zdravotnictví zahrnuje platbu za státní pojištěnce (děti, důchodce aj.). Úroky z dluhu a finanční operace: hlavně úroky ze státního dluhu. Státní sociální podpora zahrnuje i dávky pěstounské péče. Výsluhy: zvláštní sociální dávky vojáků, policistů a dalších sborů. Příspěvek na péči aj.: příspěvek na péči a ostatní dávky sociálního zabezpečení.")
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
    title = "Veřejný dluh ČR s projekcí do roku 2031", # "Veřejný dluh ČR od roku 1995 s projekcí do roku 2031"
    subtitle = "Hrubý dluh státu, samospráv a zdravotních pojišťoven", # "Hrubý dluh vládního sektoru v miliardách Kč a jako podíl na HDP"
    tabs = list(list(id = "podil", label = "% HDP"), list(id = "kc", label = "Nominální")),
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "podil", label = "Dluh (% HDP)", format = "percent", tab = "podil"),
        list(type = "y", field = "mld", label = "Dluh", unit = "mld. Kč", tab = "kc"),
        list(type = "linetype", field = "data", label = "Údaj")
    ),
    source = list(eurostat_source("gov_10dd_edpt1", "Government deficit/surplus, debt and associated data"),
                  other_source("MMF", "World Economic Outlook, October 2026 (SDMX API)", "https://data.imf.org/")),
    options = list(annotations = list(list(y = 0.6, tab = "podil", label = "Maastrichtské kritérium fiskální stability"))),
    data_note = "Skutečnost z dat Eurostatu, projekce MMF" # str_glue("Hrubý (maastrichtský) dluh vládního sektoru: stát, kraje a obce, zdravotní pojišťovny. Státní dluh v užším smyslu (dluh státního rozpočtu) je o něco nižší. Do roku {rok_dluh} Eurostat, poté projekce MMF (World Economic Outlook, říjen 2026).")
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
    transmute(zeme, year = rok, dluh = round(dluh / 100, 3), cesko = zeme == "Česko")
stopifnot(nrow(srovnani_dluh) == length(zeme_eurostat_dluh) + length(zeme_imf_dluh))

write_chart(
    df = srovnani_dluh,
    id = "rozpocet-dluh-srovnani",
    type = "bar",
    title = "Srovnání veřejného dluhu", # str_glue("Veřejný dluh ve srovnání se světem v roce {rok_srovnani}")
    subtitle = str_glue("Hrubý dluh státu, samospráv a jiných státních organizací v roce {max(srovnani_dluh$year)}"), # "Hrubý dluh vládního sektoru jako podíl na HDP; Japonsko a Turecko mají nejvyšší a nejnižší dluh v OECD"
    aes = list(
        list(type = "x", field = "zeme", label = "Země"),
        list(type = "y", field = "dluh", label = "Dluh (% HDP)", format = "percent"),
        list(type = "highlight", field = "cesko")
    ),
    source = list(eurostat_source("gov_10dd_edpt1", "Government deficit/surplus, debt and associated data"), imf_source),
    options = list(horizontal = TRUE, sort = "desc"),
    data_note = "Evroské země a EU z dat Eurostatu, ostatní z dat MMF. Svět: vážený průměr z dat MMF podle velikosti HDP." # str_glue("Hrubý dluh vládního sektoru v roce {rok_srovnani}. Evropské země (bez Norska) a EU: Eurostat; ostatní: MMF (World Economic Outlook, část hodnot jsou odhady MMF; svět je průměr vážený HDP).")
)

# ---------- (8) Růst dluhu: změna v procentních bodech HDP za posledních 5 let dat ----------
rok_rust <- rok_srovnani - 5
rust_dluhu <- dluh_zemi |>
    filter(rok %in% c(rok_rust, rok_srovnani)) |>
    pivot_wider(names_from = rok, values_from = dluh, names_prefix = "r") |>
    transmute(zeme, zmena = round((.data[[str_c("r", rok_srovnani)]] - .data[[str_c("r", rok_rust)]]) / 100, 3),
              year = rok_srovnani, cesko = zeme == "Česko")
stopifnot(!anyNA(rust_dluhu))

write_chart(
    df = rust_dluhu,
    id = "rozpocet-dluh-rust",
    type = "bar",
    title = "Srovnání růstu veřejného dluhu", # "Jak rychle roste veřejný dluh"
    subtitle = str_glue("Nárůst dluhu státu, samospráv a jiných státních organizací jako % HDP mezi lety {rok_rust} a {rok_srovnani}"), # str_glue("Změna hrubého dluhu vládního sektoru v procentních bodech HDP mezi lety {rok_rust} a {rok_srovnani}")
    aes = list(
        list(type = "x", field = "zeme", label = "Země"),
        list(type = "y", field = "zmena", label = "Změna dluhu", format = "percent", unit = "p. b."),
        list(type = "highlight", field = "cesko")
    ),
    source = list(eurostat_source("gov_10dd_edpt1", "Government deficit/surplus, debt and associated data"), imf_source),
    options = list(horizontal = TRUE, sort = "desc"),
    data_note = "Evropské země a EU z dat Eurostatu, ostatní MMF. Svět: vážený průměr z dat MMF podle velikosti HDP" # str_glue("Rozdíl mezi hrubým dluhem vládního sektoru (% HDP) v roce {rok_srovnani} a {rok_rust}. Evropské země (bez Norska) a EU: Eurostat; ostatní: MMF.")
)
