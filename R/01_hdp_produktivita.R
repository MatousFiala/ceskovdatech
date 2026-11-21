library(tidyverse)
source("./R/utils.R")

# HDP (ČSÚ)
hdp_csu <- read_csv("./data/czso_WNUC03C.csv") |> 
    filter(Ukazatel == "Hrubý domácí produkt") |> 
    mutate(Hodnota = Hodnota/1e3) |> 
    select(year = Roky, gdp_mld = Hodnota)

write_chart(
    df = hdp_csu,
    id = "hdp-realne",
    type = "line",
    title = "HDP České republiky od roku 1990",
    subtitle = "Hrubý domácí produkt očištěný od inflace, v cenách roku 2020",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "gdp_mld", label = "HDP", format = "number", unit = "mld. Kč")
    ),
    source = list(list(name = "ČSÚ", 
                  dataset = "Hrubý domácí produkt - mil. Kč, srovnatelné ceny roku 2020",
                  url = "https://data.csu.gov.cz/datastat/data/VYBER/WNUC03CT01?vSet=1&vSel=1",
                  retrieved = format(Sys.Date()))),
    data_note = ""
)

# HDP na osobu (splice ČSÚ a Maddison)
pop <- read_csv("./data/czso_WOBYG.csv") |> 
    filter(Ukazatel == "Počet obyvatel k 1. 7.") |> 
    select(year = Roky, pop = Hodnota)
hdp_os_csu <- hdp_csu |> 
    inner_join(pop, by = "year") |> 
    mutate(gdp_pc = gdp_mld*1e9/pop, source = "ČSÚ")  |> 
    select(year, gdp_pc, source)
hdp_os_mad <- read_csv("./data/maddison_gdp_pc.csv") |> 
    filter(Entity == "Czechoslovakia", Year <= 1990)  |> 
    select(year = Year, gdp_pc = `GDP per capita`)
csu_mad_factor <- hdp_os_csu$gdp_pc[hdp_os_csu$year == 1990] / hdp_os_mad$gdp_pc[hdp_os_mad$year == 1990]
hdp_os_all <- hdp_os_mad  |> 
    mutate(gdp_pc = gdp_pc*csu_mad_factor, source = "Maddison") %>%
    bind_rows(hdp_os_csu, .)

write_chart(
            df = hdp_os_all,
            id = "hdp-na-obyvatele",
            type = "line",
            title = "Dlouhodobý vývoj HDP na obyvatele",
            subtitle = "Hrubý domácí produkt na obyvatele očištěný od inflace, v cenách roku 2020",
            aes = list(
                       list(type = "x", field = "year", label = "Rok", format = "year"),
                       list(type = "y", field = "gdp_pc", label = "HDP na osobu", format = "number", unit = "Kč"),
                       list(type = "linetype", field = "source", label = "Zdroj dat")
                       ),
            source = list(
                          list(name = "ČSÚ", dataset = "Hrubý domácí produkt - mil. Kč, srovnatelné ceny roku 2020", url = "https://data.csu.gov.cz/datastat/data/VYBER/WNUC03CT01?vSet=1&vSel=1", retrieved = format(Sys.Date())),
                          list(name = "ČSÚ", dataset = "Obyvatelstvo - dlouhodobé časové řady za Českou republiku", url = "https://data.csu.gov.cz/datastat/data/VYBER/WOBYG?vSet=2&vSel=1", retrieved = format(Sys.Date())),
                          list(name = "Maddison Historical Statistics", dataset = "Maddison Project Database 2023", url = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2023", retrieved = format(Sys.Date()))
                          ),
            data_note = "Od roku 1990 data ČSÚ, předtím odhad na základě tempa růstu československé ekonomiky podle databáze Maddison. Historická čísla jsou spíše hrubým odhadem: Maddison používá jinou metodiku než ČSÚ a Československo kvůli dohánějícímu růstu Slovenska nejspíše rostlo rychleji než Česko samotné."
)

## Růst HDP
rust <- read_csv("./data/czso_NUC06R.csv") |> 
    filter(Ukazatel == "Reálný růst/pokles hrubého domácího produktu v % (SOPR = 100)", Uz012 == "CZ") |> 
    mutate(Hodnota = Hodnota / 100) |> 
    select(year = Roky, gdp_growth = Hodnota)

write_chart(
    df = rust,
    id = "hdp-rust",
    type = "bar",
    title = "Růst české ekonomiky od roku 1990",
    subtitle = "[Reálný::Očištěný od inflace] meziroční růst českého HDP, %",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "gdp_growth", label = "Růst HDP", format = "percent")
    ),
    source = list(list(name = "ČSÚ", 
                  dataset = "Národní účty – hlavní souhrnné ukazatele HDP - roční údaje",
                  url = "https://data.csu.gov.cz/datastat/data/VYBER/NUC06RT01?vSet=2&vSel=1",
                  retrieved = format(Sys.Date()))),
    data_note = ""
)
# Regiony: HDP na obyvatele
reg_hdp <- read_csv("./data/czso_NUC06R.csv")  |> 
    filter(Ukazatel == "Hrubý domácí produkt na 1 obyvatele v Kč (běžné ceny)", Odvětví == "Celkem", str_detect(Uz012, "CZ0\\d\\d")) |> 
    filter(Roky == max(Roky, na.rm = T)) |> 
    select(year = Roky, gdp_pc = Hodnota, kraj_kod = Uz012, kraj_name = `ČR, Reg. soudržnosti, Kraje` )
write_chart(
    df = reg_hdp,
    id = "hdp-regiony",
    type = "map",
    title = "HDP na obyvatele podle kraje",
    subtitle = str_glue("Krajský hrubý domácí produkt na obyvatele v roce {max(reg_hdp$year)}, [běžné ceny::Bez přepočtu na jednotnou cenovou hladinu pro srovnávání mezi lety nebo měnami]"),
    aes = list(
        list(type = "geometry", field = "kraj_kod", label = "Kraj", labeller = "kraj_name"),
        list(type = "fill", field = "gdp_pc", label = "HDP na obyvatele", format = "percent")
    ),
    source = list(list(name = "ČSÚ", 
                  dataset = "Národní účty – hlavní souhrnné ukazatele HDP - roční údaje",
                  url = "https://data.csu.gov.cz/datastat/data/VYBER/NUC06RT01?vSet=2&vSel=1",
                  retrieved = format(Sys.Date()))),
    data_note = ""
)



# Srovnání HDP s jinými zeměmi
srov <- read_csv("./data/wdi_gdp_pc_ppp.csv")
kody <- enframe(c("Česko" = "CZE", "Polsko" = "POL", "Slovensko" = "SVK", "Německo" = "DEU", "Velká Británie" = "GBR", "USA" = "USA", "Čína" = "CHN", "Indie" = "IND", "Norsko" = "NOR", "Ukrajina" = "UKR", "Subsaharská Afrika" = "SSF", "Středoafrická republika" = "CAF", "Vietnam" = "VNM", "Estonsko" = "EST", "Slovinsko" = "SVN", "Evropská unie" = "EUU", "Evropa a Střední Asie" = "ECS", "Blízký východ a severní Afrika" = "MEA", "Latinská Amerika a Karibik" = "LCN", "Svět" = "WLD"))
srov_nyni <- srov |> 
    filter(year == max(srov$year)) |> 
    inner_join(kody, by = c("iso3c" = "value")) |> 
    mutate(czechia = iso3c == "CZE", gdp_pc_ppp = gdp_pc_ppp*12.07) |> 
    select(country = name, gdp_pc_ppp, czechia) |> 
    arrange(desc(gdp_pc_ppp))

write_chart(
    df = srov_nyni,
    id = "hdp-srovnani-cross-section",
    type = "bar",
    title = "HDP na osobu ve srovnání s vybranými zeměmi",
    subtitle = str_glue("HDP na osobu v roce {max(srov$year)} podle [parity kupní síly::Přepočet, který zohledňuje, že si za stejnou částku lze v různých zemích pořídit různé množství zboží a služeb], přepočteno na koruny"),
    aes = list(
        list(type = "x", field = "country", label = "Země"),
        list(type = "y", field = "gdp_pc_ppp", label = "HDP PPP na osobu", format = "number", unit = "Kč"),
        list(type = "highlight", field = "czechia")
    ),
    source = list(list(name = "World Bank", 
                  dataset = "World Development Indicators",
                  url = "https://databank.worldbank.org/source/world-development-indicators",
                  retrieved = format(Sys.Date()))),
    data_note = "Mezinárodní dolary v cenách roku 2021 (Světová banka), pro představu přepočtené na koruny v poměru 1 $ = 12,07 Kč."
)

# Srovnání HDP s jinými zeměmi
srov_cas <- srov  |> 
    inner_join(kody, by = c("iso3c" = "value")) |> 
    filter(iso3c %in% c("CZE", "POL", "DEU", "EUU", "USA", "WLD")) |>
    mutate(gdp_pc_ppp = gdp_pc_ppp*12.07, czechia = iso3c == "CZE") |> 
    select(country = name, year, gdp_pc_ppp, czechia) 

write_chart(
    df = srov_cas,
    id = "hdp-srovnani-casova-rada",
    type = "line",
    title = "Vývoj HDP na osobu ve srovnání s vybranými zeměmi",
    subtitle = "HDP na osobu podle [parity kupní síly::Přepočet, který zohledňuje, že si za stejnou částku lze v různých zemích pořídit různé množství zboží a služeb], očištěné od inflace, přepočteno na koruny",
    aes = list(
        list(type = "x", field = "year", label = "Rok", format = "year"),
        list(type = "y", field = "gdp_pc_ppp", label = "HDP PPP na osobu", format = "number", unit = "Kč"),
        list(type = "colour", field = "country", label = "Území"),
        list(type = "highlight", field = "czechia")
    ),
    source = list(list(name = "World Bank", 
                  dataset = "World Development Indicators",
                  url = "https://databank.worldbank.org/source/world-development-indicators",
                  retrieved = format(Sys.Date()))),
    data_note = "Mezinárodní dolary v cenách roku 2021 (Světová banka), pro představu přepočtené na koruny v poměru 1 $ = 12,07 Kč."
)


