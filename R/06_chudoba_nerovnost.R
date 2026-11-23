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

# ---------- (1) Chudoba: míry příjmové chudoby (1a) a ukazatele nedostatku (1b) ----------
miry <- bind_rows(
    eurostat("ilc_li02") |>
        filter(geo == "CZ", unit == "PC", sex == "T", age == "TOTAL", statinfo == "MED_EI", rskpovth == "B_60") |>
        transmute(year = time, mira = "Relativní hranice (60 % mediánového příjmu)", chudoba = values / 100),
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
        transmute(year = time, ukazatel = "Závažná deprivace", podil = values),
    eurostat("ilc_lvho07a") |>
        filter(geo == "CZ", unit == "PC", sex == "T", age == "TOTAL", rskpovth == "TOTAL") |>
        transmute(year = time, ukazatel = "Drahé bydlení", podil = values),
    nedostatek("ilc_mdes04", "Nečekaný výdaj"),
    nedostatek("ilc_mdes01", "Vytápění"),
    nedostatek("ilc_mdes03", "Bílkoviny")
) |>
    mutate(podil = podil / 100, ukazatel = fct_inorder(ukazatel)) |>
    arrange(ukazatel, year)

write_chart(
    df = miry,
    id = "chudoba",
    type = "line",
    title = "Míra chudoby",
    subtitle = "Podíl obyvatel ohrožených chudobou podle příjmu jejich domácnosti",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "chudoba", label = "Podíl obyvatel", format = "percent"),
        list(type = "colour", field = "mira", label = "Definice chudoby")
    ),
    source = list(eurostat_source("ilc_li02", "At-risk-of-poverty rate"),
                  eurostat_source("ilc_li22b", "At-risk-of-poverty rate anchored at a fixed moment in time (2008)")),
    data_note = "Pevná hranice nezávisí na příjmech zbytku společnosti – odpovídá hranici chudoby z roku 2008, která se v dalších letech pouze upravuje o inflaci." # "Podíl lidí v domácnostech s ekvivalizovaným disponibilním příjmem pod 60 % mediánu; relativní hranice z mediánu daného roku, pevná hranice z roku 2008 upravená jen o inflaci. Šetření EU-SILC; rok šetření, příjmy jsou za předchozí rok."
)

write_chart(
    df = ukazatele,
    id = "chudoba-ukazatele",
    type = "line",
    title = "Materiální indikátory chudoby",
    subtitle = "Podíl obyvatel v domácnostech podle ukazatele materiální chudoby",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "podil", label = "Podíl obyvatel", format = "percent"),
        list(type = "colour", field = "ukazatel", label = "Indikátor")
    ),
    source = list(eurostat_source("ilc_mdsd11", "Severe material and social deprivation rate"),
                  eurostat_source("ilc_lvho07a", "Housing cost overburden rate"),
                  eurostat_source("ilc_mdes04", "Inability to face unexpected financial expenses"),
                  eurostat_source("ilc_mdes01", "Inability to keep home adequately warm"),
                  eurostat_source("ilc_mdes03", "Inability to afford a meal with meat, chicken, fish (or vegetarian equivalent) every second day")),
    options = list(facet = TRUE),
    data_note = "Závažná deprivace: domácnost si nemůže dovolit aspoň 7 z 13 běžných položek (údaje od roku 2015). Drahé bydlení: náklady na bydlení přesahují 40 % disponibilního příjmu domácnosti. Nečekaný výdaj: částka zhruba ve výši měsíční hranice chudoby pro jednotlivce (v šetření z roku 2025 šlo o 16 800 Kč), kterou domácnost nezvládne zaplatit z vlastních zdrojů. Vytápění: domácnost si nemůže dovolit udržovat byt v dostatečném teple. Bílkoviny: domácnost si nemůže dovolit maso, rybu či rostlinnou alternativu alespoň ob den."
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
    title = "Sociální vyloučení podle SO ORP",
    subtitle = str_glue("Hodnota indexu sociálního vyloučení (ISV) a jeho dimenzí podle [SO ORP::Správní obvod obce s rozšířenou působností: území, pro které jedno větší město vykonává část státní správy (např. vydává občanské a řidičské průkazy). Česko se dělí na 206 SO ORP včetně Prahy] v roce {max(isv_orp$year)}. Vyšší\u00a0=\u00a0horší"),
    tabs = c(list(list(id = "isv", label = "ISV")), map(names(dimenze), \(d) list(id = d, label = dimenze[d]))),
    aes = c(
        list(list(type = "geometry", field = "orp_kod", label = "ORP", labeller = "orp_nazev"),
             list(type = "fill", field = "isv", label = "ISV", tab = "isv", scheme = barvy[["isv"]])),
        map(names(dimenze), \(d) list(type = "fill", field = d, label = dimenze[d], tab = d, scheme = barvy[[d]]))
    ),
    source = list(isv_source, obyvatele_source),
    options = list(geo = "orp.geojson", geo_key = "KOD_ORP"),
    data_note = "Index sociálního vyloučení (MMR) sčítá body za pět dimenzí, každou na škále 0–6: podíl lidí v exekuci, příjemců příspěvku na živobytí a na bydlení, dlouhodobě nezaměstnaných a mladých s předčasným odchodem ze vzdělávání (celkem 0–30 bodů). Hodnota za SO ORP je průměr obcí vážený počtem obyvatel." # "Index sociálního vyloučení za SO ORP v roce {max(isv_orp$year)} (MMR) a body za jeho jednotlivé dimenze (0–6), průměr obcí vážený počtem obyvatel."
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
    title = "Giniho koeficient nerovnosti",
    subtitle = "Giniho koeficient [ekvivalizovaného::Se zohledněním počtu členů domácnosti] [disponibilního::Po odečtení daní a přičtení dávek, důchodů a jiných transferů] příjmu",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "gini", label = "Giniho koeficient")
    ),
    source = eurostat_source("ilc_di12", "Gini coefficient of equivalised disposable income"),
    options = list(y_zero = FALSE),
    data_note = "0 = všichni mají stejně, 100 = veškerý příjem má jeden člověk." # "Giniho koeficient ekvivalizovaného disponibilního příjmu (0 = všichni mají stejně, 100 = vše má jeden). Šetření EU-SILC."
)

# ---------- (6) Podíl nejbohatšího 1 % na příjmech a majetku, Česko ----------
wid <- function(slug) read_csv(str_glue("./data/owid_{slug}.csv")) |> select(zeme = Entity, kod = Code, year = Year, podil = 4)
top1_prijem <- wid("income-share-top-1-before-tax-wid")
top1_majetek <- wid("wealth-share-richest-1-percent")
wid_source <- other_source("WID", "Top 1% income share (pre-tax national income) and wealth share (via Our World in Data)",
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
    title = "Příjem a majetek nejbohatšího 1 %",
    subtitle = "Podíl nejbohatšího 1 % obyvatel na všech příjmech a na veškerém majetku",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "podil", label = "Podíl",  format = "percent"),
        list(type = "colour", field = "ukazatel", label = "Ukazatel")
    ),
    source = wid_source,
    data_note = "Příjem před zdaněním; čistý majetek (tj. po odečtení dluhů). Před rokem 1989 se jedná o relativně hrubé odhady." # "Podíl nejbohatšího 1 % dospělých na národním důchodu před zdaněním a na čistém majetku domácností. Odhady World Inequality Database z daňových dat, šetření a národních účtů."
)

# ---------- (7) Srovnání zemí: Giniho koeficient disponibilních příjmů ----------
rok_gini <- 2023 # poslední rok, za který mají údaje EU a svět
zeme <- c(CZ = "Česko", SK = "Slovensko", PL = "Polsko", HU = "Maďarsko", SI = "Slovinsko", DE = "Německo",
          SE = "Švédsko", US = "USA", BR = "Brazílie", `QY-MER` = "EU", WO = "Svět")
srovnani_gini <- read_csv("./data/wid_gini.csv") |>
    filter(year == rok_gini) |>
    transmute(year, zeme = zeme[country], gini = round(value * 100, 1), cesko = country == "CZ")
stopifnot(nrow(srovnani_gini) == length(zeme), !anyNA(srovnani_gini))

write_chart(
    df = srovnani_gini,
    id = "nerovnost-gini-srovnani",
    type = "bar",
    title = "Příjmová nerovnost ve srovnání s vybranými zeměmi", # "Srovnání příjmové nerovnosti"
    subtitle = str_glue("Giniho koeficient [ekvivalizovaného::Se zohledněním počtu členů domácnosti] [disponibilního::Po odečtení daní a přičtení dávek, důchodů a jiných transferů] příjmu v roce {max(srovnani_gini$year)}"), # str_glue("Giniho koeficient disponibilních příjmů v roce {rok_gini}. 0 = všichni mají stejně, 100 = vše má jeden")
    aes = list(
        list(type = "x", field = "zeme", label = "Země"),
        list(type = "y", field = "gini", label = "Giniho koeficient"),
        list(type = "highlight", field = "cesko")
    ),
    source = other_source("WID", "Gini coefficient of post-tax disposable income, bulk download",
                          "https://wid.world/data/"),
    options = list(horizontal = TRUE, sort = "desc"),
    data_note = "0 = všichni mají stejně, 100 = veškerý příjem má jeden člověk." # str_glue("Giniho koeficient disponibilních příjmů dospělých (po zdanění a sociálních dávkách) v roce {rok_gini}, odhady World Inequality Database. WID počítá příjmy jinak než EU-SILC, proto se hodnota pro Česko liší od grafu výše. EU a svět jsou počítány jako jeden celek, takže zahrnují i rozdíly mezi zeměmi a vycházejí výš než většina jednotlivých států.")
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
    title = "Nerovnost v majetku domácností",
    subtitle = str_glue("Čistý majetek domácností podle [pětiny::Domácnosti seřazené od nejchudší po nejbohatší a rozdělené do pěti stejně velkých skupin] majetku v roce {max(jmeni$year)}"),
    tabs = list(list(id = "prumer", label = "Průměrný majetek"), list(id = "podil", label = "Podíl na majetku v Česku")),
    aes = list(
        list(type = "x", field = "kvintil", label = "Majetková pětina"),
        list(type = "y", field = "prumer", label = "Průměrný majetek", unit = "mil. Kč", tab = "prumer"),
        list(type = "y", field = "podil", label = "Podíl na majetku v Česku", format = "percent", tab = "podil")
    ),
    source = other_source("ČSÚ", "Finanční situace domácností 2022 až 2024, tab. 12: Čisté jmění domácností ČR podle kvintilů čistého jmění",
                          "https://csu.gov.cz/produkty/financni-situace-domacnosti-2022-az-2024-"),
    data_note = "Čistý majetek (tj. po odečtení dluhů). Podíl je dopočten z průměrného majetku v každé pětině (pětiny jsou stejně velké)."
)
