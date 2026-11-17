library(tidyverse)
source("./R/utils.R")

# Chudoba a nerovnost
# Viditelné texty (title, subtitle, label) jsou zatím prázdné; návrh poznámky k datům je v komentáři.

eurostat_source <- function(id, dataset) {
    list(name = "Eurostat", dataset = str_glue("{dataset} ({id})"),
         url = str_glue("https://ec.europa.eu/eurostat/databrowser/view/{id}/default/table"),
         retrieved = format(Sys.Date()))
}
other_source <- function(name, dataset, url) list(name = name, dataset = dataset, url = url, retrieved = format(Sys.Date()))
eurostat <- function(id) read_csv(str_glue("./data/eurostat_{id}.csv")) |> filter(!is.na(values))

# ---------- (1) Chudoba: míry chudoby (tab 1) a ukazatele nedostatku (tab 2) ----------
miry <- bind_rows(
    eurostat("ilc_li02") |>
        filter(geo == "CZ", unit == "PC", sex == "T", age == "TOTAL", statinfo == "MED_EI", rskpovth == "B_60") |>
        transmute(year = time, mira = "Relativní hranice (60 % mediánu daného roku)", chudoba = values / 100),
    eurostat("ilc_li22b") |>
        filter(geo == "CZ", unit == "PC", sex == "T", age == "TOTAL", rskpovth == "B_60") |>
        transmute(year = time, mira = "Pevná hranice z roku 2008", chudoba = values / 100)
) |>
    mutate(mira = fct_inorder(mira)) |>
    arrange(mira, year)

nedostatek <- function(id, nazev) {
    eurostat(id) |>
        filter(geo == "CZ", unit == "PC", hhcomp == "TOTAL", rskpovth == "TOTAL") |>
        transmute(year = time, ukazatel = nazev, podil = values)
}
ukazatele <- bind_rows(
    eurostat("ilc_mdsd11") |>
        filter(geo == "CZ", unit == "PC", sex == "T", age == "TOTAL") |>
        transmute(year = time, ukazatel = "Závažná materiální a sociální deprivace", podil = values),
    eurostat("ilc_lvho07a") |>
        filter(geo == "CZ", unit == "PC", sex == "T", age == "TOTAL", rskpovth == "TOTAL") |>
        transmute(year = time, ukazatel = "Náklady na bydlení nad 40 % příjmu", podil = values),
    nedostatek("ilc_mdes04", "Nezvládne nečekaný výdaj"),
    nedostatek("ilc_mdes01", "Nemůže dostatečně vytápět byt"),
    nedostatek("ilc_mdes03", "Nemůže si dovolit maso nebo rybu ob den")
) |>
    mutate(podil = podil / 100, ukazatel = fct_inorder(ukazatel)) |>
    arrange(ukazatel, year)

write_chart(
    df = bind_rows(miry, ukazatele),
    id = "chudoba",
    type = "line",
    title = "",
    subtitle = "",
    tabs = list(list(id = "miry", label = ""), list(id = "ukazatele", label = "")),
    aes = list(
        list(type = "x", field = "year", label = "", format = "year"),
        list(type = "y", field = "chudoba", label = "", format = "percent", tab = "miry"),
        list(type = "colour", field = "mira", label = "", tab = "miry"),
        list(type = "y", field = "podil", label = "", format = "percent", tab = "ukazatele"),
        list(type = "colour", field = "ukazatel", label = "", tab = "ukazatele")
    ),
    source = list(eurostat_source("ilc_li02", "At-risk-of-poverty rate"),
                  eurostat_source("ilc_li22b", "At-risk-of-poverty rate anchored at a fixed moment in time (2008)"),
                  eurostat_source("ilc_mdsd11", "Severe material and social deprivation rate"),
                  eurostat_source("ilc_lvho07a", "Housing cost overburden rate"),
                  eurostat_source("ilc_mdes04", "Inability to face unexpected financial expenses"),
                  eurostat_source("ilc_mdes01", "Inability to keep home adequately warm"),
                  eurostat_source("ilc_mdes03", "Inability to afford a meal with meat, chicken, fish (or vegetarian equivalent) every second day")),
    data_note = "" # "Míry chudoby: podíl lidí v domácnostech s ekvivalizovaným disponibilním příjmem pod 60 % mediánu; relativní hranice z mediánu daného roku, pevná hranice z roku 2008 upravená jen o inflaci. Závažná deprivace: domácnost si nemůže dovolit aspoň 7 z 13 běžných položek (od 2015). Náklady na bydlení: domácnosti, které na bydlení vydávají víc než 40 % disponibilního příjmu. Nečekaný výdaj ve výši měsíční hranice chudoby. Šetření EU-SILC; rok šetření, příjmy jsou za předchozí rok."
)

# ---------- Index sociálního vyloučení ----------
# MMR zveřejňuje body za obce; hodnoty za SO ORP jsou průměry obcí vážené počtem obyvatel (ověřeno na hodnotách MMR).
# Stejně se zde počítá celostátní hodnota a dimenze za ORP. Každá z pěti dimenzí má 0–6 bodů, index je jejich součet (0–30).
dimenze <- c(exekuce = "Exekuce", pnz = "Příspěvek na živobytí", pnb = "Příspěvek na bydlení",
             uoz = "Dlouhodobá nezaměstnanost", odchody = "Předčasné odchody ze vzdělávání")
barvy <- c(isv = "reds", exekuce = "oranges", pnz = "purples", pnb = "blues", uoz = "greens", odchody = "greys") # mapa: každý tab jinou barvou
isv_obce <- read_csv("./data/mmr_isv.csv") |>
    select(kod_obce = `Kód obce`, nazev_obce = `Název obce`, kod_orp = `Kód ORP`, matches("\\(rok \\d{4}\\)")) |>
    pivot_longer(-c(kod_obce, nazev_obce, kod_orp), names_to = c("ukazatel", "rok"),
                 names_pattern = "(.*) \\(rok (\\d{4})\\)", names_transform = list(rok = as.integer)) |>
    mutate(ukazatel = case_when(
        str_detect(ukazatel, "za obec") ~ "isv", str_detect(ukazatel, "za SO ORP") ~ "isv_orp",
        str_detect(ukazatel, "za kraj") ~ "isv_kraj", str_detect(ukazatel, "exekucí") ~ "exekuce",
        str_detect(ukazatel, "PnB") ~ "pnb", str_detect(ukazatel, "PnŽ") ~ "pnz",
        str_detect(ukazatel, "UoZ") ~ "uoz", str_detect(ukazatel, "odchodů") ~ "odchody")) |>
    pivot_wider(names_from = ukazatel, values_from = value) |>
    mutate(kod_orp = if_else(nazev_obce == "Praha", 1000, kod_orp)) |> # Praha nemá SO ORP, v mapě má kód 1000
    inner_join(read_csv("./data/czso_OBY01B01.csv") |> select(kod_obce = UZ24596C, rok = Roky, obyvatele = Hodnota),
               by = c("kod_obce", "rok"))

isv_source <- other_source("MMR", "Index sociálního vyloučení 2016–2024",
                           "https://mmr.gov.cz/cs/microsites/socialni-zaclenovani/index-socialniho-vylouceni/data-a-mapy")
obyvatele_source <- other_source("ČSÚ", "Základní údaje o stavu a pohybu obyvatel podle obcí a měst",
                                 "https://data.csu.gov.cz/datastat/data/VYBER/OBY01B01T01")

# (3) Česko v čase: index a zvlášť jeho dimenze
isv_cr <- isv_obce |>
    summarise(across(c(isv, names(dimenze)), \(x) weighted.mean(x, obyvatele, na.rm = TRUE)), .by = rok)

write_chart(
    df = isv_cr |> transmute(year = rok, isv),
    id = "chudoba-isv",
    type = "line",
    title = "",
    subtitle = "",
    aes = list(
        list(type = "x", field = "year", label = "", format = "year"),
        list(type = "y", field = "isv", label = "")
    ),
    source = list(isv_source, obyvatele_source),
    data_note = "" # "Index sociálního vyloučení (MMR) sčítá body za pět dimenzí, každá 0–6 bodů: podíl lidí v exekuci, příjemců příspěvku na živobytí a na bydlení, dlouhodobě nezaměstnaných a mladých s předčasným odchodem ze vzdělávání (0–30 bodů). Hodnota za Česko je průměr obcí vážený počtem obyvatel."
)

isv_dimenze <- isv_cr |>
    select(year = rok, all_of(names(dimenze))) |>
    pivot_longer(-year, names_to = "dimenze", values_to = "body") |>
    mutate(dimenze = factor(.env$dimenze[dimenze], .env$dimenze)) |>
    arrange(dimenze, year)

write_chart(
    df = isv_dimenze,
    id = "chudoba-isv-dimenze",
    type = "line",
    title = "",
    subtitle = "",
    aes = list(
        list(type = "x", field = "year", label = "", format = "year"),
        list(type = "y", field = "body", label = ""),
        list(type = "colour", field = "dimenze", label = "")
    ),
    source = list(isv_source, obyvatele_source),
    data_note = "" # "Body za jednotlivé dimenze indexu sociálního vyloučení (MMR), každá 0–6 bodů podle podílu lidí v exekuci, příjemců příspěvku na živobytí a na bydlení, dlouhodobě nezaměstnaných a mladých s předčasným odchodem ze vzdělávání. Hodnota za Česko je průměr obcí vážený počtem obyvatel."
)

# (4) Mapa SO ORP, poslední rok: index a jednotlivé dimenze (taby)
isv_orp <- isv_obce |>
    filter(rok == max(rok)) |>
    summarise(across(names(dimenze), \(x) weighted.mean(x, obyvatele, na.rm = TRUE)),
              isv = first(isv_orp), obyvatele = sum(obyvatele), .by = c(rok, kod_orp)) |>
    mutate(orp_kod = as.character(kod_orp)) |>
    left_join(read_csv("./data/czso_OBY04BOR.csv") |> distinct(orp_kod = as.character(Uz4), orp_nazev = `SO ORP`), by = "orp_kod") |>
    select(year = rok, orp_kod, orp_nazev, isv, all_of(names(dimenze)))

write_chart(
    df = isv_orp,
    id = "chudoba-isv-orp",
    type = "map",
    title = "",
    subtitle = "",
    tabs = c(list(list(id = "isv", label = "")), map(names(dimenze), \(d) list(id = d, label = ""))),
    aes = c(
        list(list(type = "geometry", field = "orp_kod", label = "", labeller = "orp_nazev"),
             list(type = "fill", field = "isv", label = "", tab = "isv", scheme = barvy[["isv"]])),
        map(names(dimenze), \(d) list(type = "fill", field = d, label = "", tab = d, scheme = barvy[[d]]))
    ),
    source = list(isv_source, obyvatele_source),
    options = list(geo = "orp.geojson", geo_key = "KOD_ORP"),
    data_note = "" # "Index sociálního vyloučení za SO ORP v roce {max(isv_orp$year)} (MMR) a body za jeho jednotlivé dimenze (0–6), průměr obcí vážený počtem obyvatel."
)

# ---------- (5) Giniho koeficient ----------
gini <- eurostat("ilc_di12") |>
    filter(geo == "CZ", age == "TOTAL") |>
    transmute(year = time, gini = values) |>
    arrange(year)

write_chart(
    df = gini,
    id = "nerovnost-gini",
    type = "line",
    title = "",
    subtitle = "",
    aes = list(
        list(type = "x", field = "year", label = "", format = "year"),
        list(type = "y", field = "gini", label = "")
    ),
    source = eurostat_source("ilc_di12", "Gini coefficient of equivalised disposable income"),
    options = list(y_zero = FALSE),
    data_note = "" # "Giniho koeficient ekvivalizovaného disponibilního příjmu (0 = všichni mají stejně, 100 = vše má jeden). Šetření EU-SILC."
)

# ---------- (6) Podíl nejbohatšího 1 % na příjmech a majetku, Česko ----------
wid <- function(slug) read_csv(str_glue("./data/owid_{slug}.csv")) |> select(zeme = Entity, kod = Code, year = Year, podil = 4)
top1_prijem <- wid("income-share-top-1-before-tax-wid")
top1_majetek <- wid("wealth-share-richest-1-percent")
wid_source <- other_source("World Inequality Database", "Top 1% income share (pre-tax national income) and wealth share, via Our World in Data",
                           "https://wid.world/country/czech-republic/")

top1_cr <- bind_rows(
    top1_prijem |> filter(kod == "CZE") |> transmute(year, ukazatel = "Podíl na příjmech", podil),
    top1_majetek |> filter(kod == "CZE") |> transmute(year, ukazatel = "Podíl na majetku", podil)
) |>
    mutate(podil = podil / 100) |>
    arrange(ukazatel, year)

write_chart(
    df = top1_cr,
    id = "nerovnost-top1",
    type = "line",
    title = "",
    subtitle = "",
    aes = list(
        list(type = "x", field = "year", label = "", format = "year"),
        list(type = "y", field = "podil", label = "", format = "percent"),
        list(type = "colour", field = "ukazatel", label = "")
    ),
    source = wid_source,
    data_note = "" # "Podíl nejbohatšího 1 % dospělých na národním důchodu před zdaněním a na čistém majetku domácností. Odhady World Inequality Database z daňových dat, šetření a národních účtů."
)

# ---------- (7) Srovnání zemí: nejbohatší 1 % (taby příjmy / majetek) ----------
zeme <- c(CZE = "Česko", SVK = "Slovensko", POL = "Polsko", HUN = "Maďarsko", SVN = "Slovinsko", DEU = "Německo",
          SWE = "Švédsko", USA = "USA", BRA = "Brazílie", EU = "EU", OWID_WRL = "Svět")
rok_prijem <- 2022 # poslední rok, za který mají příjmy všechny vybrané země
rok_majetek <- 2023 # poslední rok, za který má majetek EU
# EU je jen ve WID (Our World in Data ji nemá): agregát podle tržních kurzů, stejné ukazatele jako u zemí
eu <- read_csv("./data/wid_eu.csv") |>
    transmute(zeme = "EU", kod = "EU", year, podil = value * 100,
              variable = if_else(variable == "sptincj992", "prijem", "majetek"))
srovnani_top1 <- full_join(
    bind_rows(top1_prijem, filter(eu, variable == "prijem")) |>
        filter(year == rok_prijem) |> transmute(klic = coalesce(kod, zeme), prijem = podil / 100),
    bind_rows(top1_majetek, filter(eu, variable == "majetek")) |>
        filter(year == rok_majetek) |> transmute(klic = coalesce(kod, zeme), majetek = podil / 100),
    by = "klic"
) |>
    filter(klic %in% names(zeme)) |>
    transmute(zeme = zeme[klic], prijem, majetek, cesko = klic == "CZE")

write_chart(
    df = srovnani_top1,
    id = "nerovnost-top1-srovnani",
    type = "bar",
    title = "",
    subtitle = "",
    tabs = list(list(id = "prijem", label = ""), list(id = "majetek", label = "")),
    aes = list(
        list(type = "x", field = "zeme", label = ""),
        list(type = "y", field = "prijem", label = "", format = "percent", tab = "prijem"),
        list(type = "y", field = "majetek", label = "", format = "percent", tab = "majetek"),
        list(type = "highlight", field = "cesko")
    ),
    source = list(wid_source, other_source("World Inequality Database", "European Union (QY-MER): top 1% income and wealth share, bulk download",
                                           "https://wid.world/data/")),
    options = list(horizontal = TRUE, sort = "desc"),
    data_note = "" # "Podíl nejbohatšího 1 % dospělých na národním důchodu před zdaněním (rok {rok_prijem}) a na čistém majetku domácností (rok {rok_majetek}). World Inequality Database."
)

# ---------- (8) Čisté jmění domácností podle kvintilů jmění (HFCS) ----------
kvintily <- c(`nejnižších 20 %` = "1. pětina", `druhých 20 %` = "2. pětina", `třetích 20 %` = "3. pětina",
              `čtvrtých 20 %` = "4. pětina", `nejvyšších 20 %` = "5. pětina")
hfcs <- read_csv("./data/czso_hfcs_jmeni_kvintily.csv") |>
    filter(rok == max(rok))
jmeni <- hfcs |>
    filter(kvintil %in% names(kvintily)) |>
    pivot_wider(names_from = statistika, values_from = value) |>
    mutate(podil = prumer / sum(prumer)) |> # pětiny mají stejně domácností: podíl na celkovém jmění = podíl průměrů
    transmute(year = rok, kvintil = factor(kvintily[kvintil], kvintily),
              prumer = prumer / 1e3, podil) |>
    arrange(kvintil)

write_chart(
    df = jmeni,
    id = "nerovnost-jmeni-kvintily",
    type = "bar",
    title = "",
    subtitle = "",
    tabs = list(list(id = "prumer", label = ""), list(id = "podil", label = "")),
    aes = list(
        list(type = "x", field = "kvintil", label = ""),
        list(type = "y", field = "prumer", label = "", unit = "mil. Kč", tab = "prumer"),
        list(type = "y", field = "podil", label = "", format = "percent", tab = "podil")
    ),
    source = other_source("ČSÚ", "Finanční situace domácností 2022 až 2024, tab. 12: Čisté jmění domácností ČR podle kvintilů čistého jmění",
                          "https://csu.gov.cz/produkty/financni-situace-domacnosti-2022-az-2024-"),
    data_note = "" # "Domácnosti seřazené podle čistého jmění (majetek minus dluhy) a rozdělené na pět stejně početných skupin. Šetření Finanční situace domácností (HFCS) {max(hfcs$rok)}. Podíl na celkovém jmění všech domácností spočten z průměrů."
)
