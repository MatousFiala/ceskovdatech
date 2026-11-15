# Česko v číslech (Czechia by numbers)
Češko v číslech is a website which aims to show the most important data about Czechia is a simple, easy-to-reuse format. It takes inspiration from *ukbynumbers.org* and *ourworldindata.org*. It contains a list of thematic articles which display 5-10 of the most important charts one needs to understand the topic. 

## Topics

### Obyvatelstvo
Základní demografické údaje Věková struktura a porodnost
Migrace
Vzdělání

### Ekonomika
HDP, růst a produktivita
Veřejné finance a daně
Mzdy a zaměstnanost
Důchody

### Energetika a životní prostředí
Energetický mix 
Emise skleníkových plynů

### Zdravotnictví
Naděje dožití

### Sociální
Chudoba
Nerovnost
Exekuce

## Data
`Rscript run_all.R` (from the project root) downloads the newest data into `data/`, one CSV per source dataset. Each script in `R/` can also be run on its own. Data is saved raw; derived series (e.g. real GDP per capita) are computed later, at the chart stage.

Needs the R packages `tidyverse`, `czso2`, `eurostat` and `WDI`.

| Chart | Script | File in `data/` |
|---|---|---|
| HDP reálně (1990+) | `R/czso.R` | `czso_WNUC03C.csv` |
| HDP na osobu | `R/czso.R` | `czso_WNUC03C.csv` ÷ `czso_WOBYG.csv` (počet obyvatel k 1. 7.) |
| Růst HDP, nominální HDP, kraje | `R/czso.R` | `czso_NUC06R.csv` |
| HDP před rokem 1990 | `R/world.R` | `maddison_gdp_pc.csv` |
| Produktivita práce (osoba, hodina) | `R/eurostat.R` | `eurostat_nama_10_lp_ulc.csv` |
| Srovnání HDP na osobu (PPP) | `R/world.R` | `wdi_gdp_pc_ppp.csv`, `maddison_gdp_pc.csv` |
| Míra (ne)zaměstnanosti, i podle krajů | `R/czso.R` | `czso_ZAM01.csv` |
| Srovnání (ne)zaměstnanosti v EU | `R/eurostat.R` | `eurostat_une_rt_a.csv`, `eurostat_lfsi_emp_a.csv` |
| Průměrná a mediánová mzda | `R/czso.R` | `czso_ZAMG08.csv` |
| Mediánová mzda podle krajů | `R/czso.R` | `czso_WREG0404.csv` |
| Distribuce mezd | — | TODO: no machine-readable source found yet |

Notes:
- `czso2` workarounds: `czso_download_dataset()` currently saves metadata instead of data, so full datasets are read straight from `data.csu.gov.cz/opendata/sady/{ID}/distribuce/csv`; `czso_get_table()` is only reliable with `format = "CSV"`.
- The World Bank API is slow: `R/world.R` can take several minutes.
- Maddison ends in 2022 and is in 2011 international dollars; Czechia starts in 1970, Czechoslovakia in 1820.
- Median wages exist only from 2011 (by region from 2015).

## Drafts

### HDP, růst a produktivita
ČŘ České HDP (1990+ ČSÚ, dříve maddison -- v reálných Korunách)
ČŘ České HDP na osobu (-//-)
ČŘ Růst HDP 
Produktivita práce (na osobu, na hodinu práce) -- ČSÚ?
Srovnání HDP na osobu s jinými zeměmi (PPP USD) -- WBI asi
Srovnání vývoje HDP s jinými zeměmi (PPP USD) -- stejná data jak <-, jen focus i na minulost.

### Mzdy a zaměstnanost 
ČŘ Míra nezaměstnanosti
ČŘ Míra zaměstnanosti
MAPA nezaměstnanost
SLOUP Srovnání míry z. a nz. (v rámci EU -- Eurostat?)
ČŘ Mediánová mzda
SLOUP distribuce mezd
MAPA mediánová (průměrná jestli není) mzda podle Kraje









