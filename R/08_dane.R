library(tidyverse)
source("./R/utils.R")

# Daňový systém (articles/danovy_system.md): daně
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
    title = "Daňové příjmy", # "Kolik státu přinášejí jednotlivé daně"
    subtitle = "Daňové příjmy veřejného sektoru podle druhu daně", # "Daňové příjmy vládního sektoru v ČR podle druhu daně"
    tabs = list(list(id = "realne", label = "Reálné (ceny roku 2020)"), list(id = "nominalni", label = "Nominální"), list(id = "podil", label = "% daňových příjmů")),
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "mld_2020", label = "Reálné příjmy", unit = "mld. Kč", tab = "realne"),
        list(type = "y", field = "mld", label = "Nominální příjmy", unit = "mld. Kč", tab = "nominalni"),
        list(type = "y", field = "podil", label = "% daňových příjmů", format = "percent", tab = "podil"),
        list(type = "fill", field = "dan", label = "Daň")
    ),
    source = list(oecd_source, other_source("ČSÚ", "Míra inflace (CEN0101H)", "https://data.csu.gov.cz/datastat/data/VYBER/CEN0101HT01"),
                  other_source("Světová banka", "Inflation, consumer prices (FP.CPI.TOTL.ZG)",
                               "https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG?locations=CZ")),
    options = list(facet = TRUE),
    data_note = "Daně vybrané v celém veřejném sektoru, včetně těch, které plynou mimo státní rozpočet. Reálné částky jsou přepočtené na ceny roku 2020 (inflace podle ČSÚ, před rokem 1997 podle Světové banky)." # "Daně vybrané všemi úrovněmi vlády (stát, kraje, obce, zdravotní pojišťovny) podle metodiky OECD. Sociální a zdravotní pojištění zahrnuje pojistné zaměstnanců, zaměstnavatelů a OSVČ. Ostatní daně: hlavně silniční daň, cla, poplatky a daně z hazardu. Ceny roku 2020 podle průměrné roční míry inflace (ČSÚ, před rokem 1997 Světová banka)."
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
    title = "Skladba daní ve srovnání s vybranými zeměmi", # str_glue("Z jakých daní žijí státy, rok {rok_dane}")
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
    data_note = "Daně vybrané v celém veřejném sektoru, včetně těch, které plynou mimo státní rozpočet. Průměr OECD: nevážený průměr členských zemí. DPH zahrnuje i ostatní všeobecné daně ze zboží a služeb, hlavně daň z obratu (sales tax) v USA, která tam DPH nahrazuje." # str_glue("Rok {rok_dane}. Daně a pojistné vybrané všemi úrovněmi vlády podle metodiky OECD. Průměr OECD je nevážený průměr členských zemí. Ve skandinávských zemích se sociální systém financuje hlavně z daně z příjmů, proto mají nízký podíl pojistného; v Dánsku pojistné téměř neexistuje.")
)

# ---------- Zdanění práce podle výše mzdy (3–5) ----------
oecd_tw_source <- other_source("OECD", "Taxing Wages", "https://www.oecd.org/en/topics/sub-issues/tax-policy/taxing-wages.html")
taxing_wages <- read_csv("./data/oecd_taxing_wages.csv")
urovne_mzdy <- c(AW50 = "50 %", AW67 = "67 %", AW100 = "100 %", AW133 = "133 %", AW167 = "167 %", AW200 = "200 %", AW250 = "250 %")
slozky_klinu <- c(ERSSC = "Pojistné zaměstnavatele", EESSC = "Pojistné zaměstnance", IT_CG = "Daň z příjmů")
# Jen svobodný bez dětí a jen daň z příjmů a pojistné, bez dávek (kterými OECD klín snižuje): zdanění práce samotné

# (3) Zdanění práce v Česku podle výše mzdy: složky jako % nákladů práce (hrubá mzda + pojistné zaměstnavatele).
# V korunách OECD vede daně a pojistné záporně
klin_mzda <- taxing_wages |>
    filter(kod == "CZE", jednotka == "XDC", domacnost == "S_C0") |>
    select(-jednotka) |>
    pivot_wider(names_from = ukazatel, values_from = hodnota) |>
    mutate(naklady_prace = GWE - ERSSC, mzda_mesicne = round(GWE / 12)) |>
    pivot_longer(names(slozky_klinu), names_to = "slozka", values_to = "kc") |>
    transmute(year = rok, mzda = factor(urovne_mzdy[mzda], urovne_mzdy),
              mzda_kc = paste(formatC(round(mzda_mesicne, -3), format = "d", big.mark = "\u00a0"), "Kč"), # záložka Mzda: hrubá měsíční mzda, na tisíce
              slozka = factor(slozky_klinu[slozka], slozky_klinu), podil = round(-kc / naklady_prace, 4)) |>
    arrange(mzda, slozka)
# kontrola: svobodný bez dětí nedostává dávky, složky tedy dávají celkový klín OECD
klin_kontrola <- taxing_wages |> filter(kod == "CZE", ukazatel == "AV_TW", domacnost == "S_C0") |>
    transmute(mzda = factor(urovne_mzdy[mzda], urovne_mzdy), klin = hodnota / 100) |>
    left_join(summarise(klin_mzda, soucet = sum(podil), .by = mzda), by = "mzda")
stopifnot(nrow(klin_mzda) == length(urovne_mzdy) * length(slozky_klinu), !anyNA(klin_mzda), all(klin_mzda$podil >= 0),
          nrow(klin_kontrola) == length(urovne_mzdy), all(abs(klin_kontrola$klin - klin_kontrola$soucet) < 0.001))

write_chart(
    df = klin_mzda,
    id = "dane-klin-mzda",
    type = "bar",
    title = "Zdanění práce podle výše mzdy", # "Kolik ze mzdy si vezme stát"
    subtitle = str_glue("Daň z příjmů a odvody jako podíl na [nákladech práce::Vše, co zaměstnavatel platí za zaměstnance: hrubá mzda a odvody] při mzdě od 50 % do 250 % průměrné mzdy v roce {max(klin_mzda$year)}. Odvody zaměstnavatele se nepočítají do hrubé mzdy, jedná se však stále o danění práce."), # "Daň z příjmů a pojistné jako podíl na celkových nákladech zaměstnavatele"
    aes = list(
        list(type = "x", field = "mzda_kc", label = "Mzda", tab = "kc"),
        list(type = "x", field = "mzda", label = "% průměrné mzdy", tab = "procenta"),
        list(type = "y", field = "podil", label = "Celková sazba", format = "percent"),
        list(type = "fill", field = "slozka", label = "Složka")
    ),
    source = oecd_tw_source,
    tabs = list(list(id = "kc", label = "Mzda"), list(id = "procenta", label = "% průměrné mzdy")),
    data_note = str_glue("Náklady práce = hrubá mzda + odvody placené zaměstnavatelem. Počítá se se svobodným pracovníkem bez dětí.") # návrh
)

# (4) Zdanění práce ve srovnání zemí, bodový graf: svobodný bez dětí s 50, 100 a 250 % průměrné mzdy;
# země jako (2). Daň z příjmů a pojistné = klín OECD bez dávek (CB je v % nákladů práce záporné, klín snižuje).
# Řádky seřazené podle zdanění při 50 % průměrné mzdy, nejnižší nahoře (options$sort_by)
klin_srovnani <- taxing_wages |>
    filter(ukazatel %in% c("AV_TW", "CB"), jednotka == "PT_COS_LB", domacnost == "S_C0", mzda %in% c("AW50", "AW100", "AW250")) |>
    pivot_wider(names_from = ukazatel, values_from = hodnota) |>
    transmute(year = rok, zeme = zeme[kod], mzda = factor(urovne_mzdy[mzda], urovne_mzdy[c("AW50", "AW100", "AW250")]),
              klin = round((AV_TW - CB) / 100, 4), cesko = kod == "CZE") |>
    arrange(zeme, mzda)
stopifnot(n_distinct(klin_srovnani$zeme) == length(zeme), nrow(klin_srovnani) == 3 * length(zeme), !anyNA(klin_srovnani))
# záložka Progrese: rozdíl zdanění při 250 % a 50 % průměrné mzdy (p. b.), vlastní řádky bez mzdy, seřazené od nejmenšího
progrese_srovnani <- klin_srovnani |>
    filter(mzda %in% c("50 %", "250 %")) |>
    pivot_wider(names_from = mzda, values_from = klin) |>
    transmute(year, zeme = as.character(zeme), progrese = round(`250 %` - `50 %`, 4), cesko) |>
    arrange(progrese)
stopifnot(nrow(progrese_srovnani) == length(zeme), !anyNA(progrese_srovnani))

write_chart(
    df = bind_rows(mutate(klin_srovnani, zeme = as.character(zeme)), progrese_srovnani),
    id = "dane-klin-srovnani",
    type = "dot",
    title = "Zdanění práce ve srovnání s vybranými zeměmi", # "Jak progresivně země daní práci"
    subtitle = str_glue("Daň z příjmů a odvody jako podíl na [nákladech práce::Vše, co zaměstnavatel platí za zaměstnance: hrubá mzda a odvody] při mzdě 50 %, 100 % a 250 % průměrné mzdy v roce {max(klin_srovnani$year)}"), # "Zdanění práce při nízké, průměrné a vysoké mzdě"
    tabs = list(list(id = "klin", label = "Celková sazba"), list(id = "progrese", label = "Rozdíl v sazbě mezi 50 % a 250 % průměrné mzdy", type = "bar")),
    aes = list(
        list(type = "x", field = "zeme", label = "Země"),
        list(type = "y", field = "klin", label = "Celková sazba", format = "percent", tab = "klin"),
        list(type = "y", field = "progrese", label = "Rozdíl v sazbě mezi 50 % a 250 % průměrné mzdy", format = "percent", unit = "p. b.", tab = "progrese"),
        list(type = "colour", field = "mzda", label = "% průměrné mzdy", tab = "klin"),
        list(type = "highlight", field = "cesko")
    ),
    source = oecd_tw_source,
    options = list(range = c("50 %", "100 %", "250 %"), sort_by = "50 %", colour_scheme = "blues", facet = TRUE, facet_scale = "shared"),
    data_note = "Náklady práce = hrubá mzda + odvody placené zaměstnavatelem. Počítá se se svobodným pracovníkem bez dětí. Průměr OECD: nevážený průměr členských zemí. Německo: kvůli stropu odvodů je mzda ve výši 250 % průměru zdaněna méně než průměrná mzda, proto bod leží mimo čáru." # návrh
)

# (5) Vývoj daňového klínu v Česku od roku 2000 při čtyřech výších mzdy, svobodný bez dětí; druhá záložka: rozdíl
# klínu při 167 % a 67 % průměrné mzdy v procentních bodech (míra progresivity), jako vlastní řádky bez mzdy
klin_vyvoj <- read_csv("./data/oecd_taxing_wages_cz.csv") |>
    filter(ukazatel == "TW") |>
    transmute(year = rok, mzda = factor(urovne_mzdy[mzda], urovne_mzdy[c("AW67", "AW100", "AW133", "AW167")]), klin = round(hodnota / 100, 4)) |>
    arrange(mzda, year)
stopifnot(n_distinct(klin_vyvoj$mzda) == 4, !anyNA(klin_vyvoj))
klin_rozdil <- klin_vyvoj |>
    filter(mzda %in% c("67 %", "167 %")) |>
    pivot_wider(names_from = mzda, values_from = klin) |>
    transmute(year, rozdil = round(`167 %` - `67 %`, 4))
stopifnot(nrow(klin_rozdil) == n_distinct(klin_vyvoj$year), !anyNA(klin_rozdil))

write_chart(
    df = bind_rows(klin_vyvoj, klin_rozdil),
    id = "dane-klin-vyvoj",
    type = "line",
    title = "Vývoj zdanění práce", # "Jak se měnilo zdanění práce"
    subtitle = "Daň z příjmů a odvody jako podíl na [nákladech práce::Vše, co zaměstnavatel platí za zaměstnance: hrubá mzda a odvody] při mzdě od 67 % do 167 % průměrné mzdy", # "Daňový klín v Česku od roku 2000"
    tabs = list(list(id = "klin", label = "Celková sazba"), list(id = "rozdil", label = "Rozdíl v sazbě mezi 67 % a 167 % průměrné mzdy")),
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "klin", label = "Celková sazba", format = "percent", tab = "klin"),
        list(type = "y", field = "rozdil", label = "Rozdíl v sazbě mezi 67 % a 167 % průměrné mzdy", format = "percent", unit = "p. b.", tab = "rozdil"),
        list(type = "colour", field = "mzda", label = "% průměrné mzdy", tab = "klin")
    ),
    source = oecd_tw_source,
    options = list(y_zero = FALSE, colour_scheme = "blues", annotations = list(list(x = 2006, label = "Zavedení slevy na poplatníka"), list(x = 2020, label = "Zrušení superhrubé mzdy"))),
    data_note = "Náklady práce = hrubá mzda + odvody placené zaměstnavatelem. Počítá se se svobodným pracovníkem bez dětí." # návrh
)
