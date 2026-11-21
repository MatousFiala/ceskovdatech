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

Needs the R packages `tidyverse`, `czso2`, `eurostat`, `WDI` and `statnipokladna`.

| Chart | Script | File in `data/` |
|---|---|---|
| HDP reálně (1990+) | `R/czso.R` | `czso_WNUC03C.csv` |
| HDP na osobu | `R/czso.R` | `czso_WNUC03C.csv` ÷ `czso_WOBYG.csv` (počet obyvatel k 1. 7.) |
| Růst HDP, nominální HDP, kraje | `R/czso.R` | `czso_NUC06R.csv` |
| HDP před rokem 1990 | `R/world.R` | `maddison_gdp_pc.csv` |
| Produktivita práce (osoba, hodina) v Kč | `R/czso.R` | `czso_WNUC03C.csv` ÷ `czso_nu_M0131a.csv` (zaměstnané osoby), `czso_nu_M0131c.csv` (odpracované hodiny, v tisících); řádky `activity == "_T"` |
| Produktivita práce, hotové ukazatele ČSÚ | `R/czso.R` | `czso_produktivita_prace.csv` |
| Produktivita práce, srovnání s EU (PPS) | `R/eurostat.R` | `eurostat_nama_10_lp_ulc.csv` |
| Srovnání HDP na osobu (PPP) | `R/world.R` | `wdi_gdp_pc_ppp.csv`, `maddison_gdp_pc.csv` |
| Míra (ne)zaměstnanosti, i podle krajů | `R/czso.R` | `czso_ZAM01.csv` |
| Míra nezaměstnanosti podle věku | `R/czso.R`, `R/eurostat.R` | `czso_ZAM04.csv`, váhy pro sloučení skupin z `eurostat_lfsa_agan.csv` |
| Počet obyvatel, přirozený přírůstek, migrace | `R/czso.R` | `czso_WOBYG.csv` |
| Obyvatelstvo podle krajů a SO ORP | `R/czso.R` | `czso_OBY01B.csv` |
| Obyvatelstvo podle velikosti obce | `R/czso.R` | `czso_RSO02A01.csv` |
| Obyvatelé podle věku (pyramida, medián věku, věkové skupiny, index závislosti) | `R/czso.R`, `R/eurostat.R` | `czso_OBY02BHVD.csv` (od 2000), před rokem 2000 `eurostat_demo_pjan.csv`; projekce `czso_OBY02PROJ1A.csv` (střední varianta) |
| Průměrný věk a věkové složení krajů | `R/czso.R` | `czso_OBY02FHVD.csv` |
| Úhrnná plodnost, průměrný věk matky při prvním porodu, podíl narozených mimo manželství | `R/czso.R`, `R/eurostat.R` | `czso_OBY03HVD.csv` (od 2000), před rokem 2001 `eurostat_demo_find.csv` |
| Narození podle věku matky | `R/czso.R`, `R/eurostat.R` | `czso_OBY03B.csv` (od 2001), před rokem 2001 `eurostat_demo_fagec.csv` |
| Naděje dožití ČR (od 1920), SO ORP (pětiletá období) | `R/czso.R` | `czso_OBY04BCR.csv`, `czso_OBY04BOR.csv` (uložena jen naděje dožití při narození) |
| Kojenecká úmrtnost | `R/czso.R` | do roku 2000 `czso_pohyb_1919.csv` (tabulka 1919–2023 z publikace ČSÚ, Excel), od 2001 `czso_OBY04H.csv` ÷ živě narození z `czso_OBY03HVD.csv` |
| Příčiny smrti | `R/czso.R` | `czso_OBY04C.csv` (uložena jen ČR, všechny věky) |
| Zdraví podle příjmu (EU-SILC) | `R/eurostat.R` | `eurostat_hlth_silc_10.csv` |
| Mateřská úmrtnost, nadváha, obezita, kouření | `R/world.R` | `who_gho.csv` (WHO Global Health Observatory) |
| Sebevraždy | `R/world.R` | `owid_suicide_who_mdb.csv` (WHO Mortality Database přes Our World in Data) |
| Znečištění ovzduší (PM2.5) | `R/world.R` | `owid_pm25_air_pollution.csv` (SatPM, Washington University in St. Louis, přes Our World in Data) |
| Úmrtí a DALY podle rizikových faktorů | ručně z [GBD Results tool](https://vizhub.healthdata.org/gbd-results/) | `IHME-GBD_2023_DATA-1bb22f58-1.zip` (GBD 2023: Česko, obě pohlaví, všechny věky, všechny příčiny, rizikové faktory 2. úrovně, 2023) |
| Chudoba (relativní, s pevnou hranicí), deprivace, náklady na bydlení, nečekané výdaje, vytápění, jídlo, Gini | `R/eurostat.R` | `eurostat_ilc_li02.csv`, `eurostat_ilc_li22b.csv`, `eurostat_ilc_mdsd11.csv`, `eurostat_ilc_lvho07a.csv`, `eurostat_ilc_mdes04.csv`, `eurostat_ilc_mdes01.csv`, `eurostat_ilc_mdes03.csv`, `eurostat_ilc_di12.csv` (Česko a EU27) |
| Index sociálního vyloučení (obce, SO ORP) | `R/mmr.R`, `R/czso.R` | `mmr_isv.csv` (MMR); váhy: počet obyvatel obcí `czso_OBY01B01.csv` (uložen jen počet obyvatel k 1. 7., od 2016) |
| Podíl nejbohatšího 1 % na příjmech a majetku | `R/world.R` | `owid_income-share-top-1-before-tax-wid.csv`, `owid_wealth-share-richest-1-percent.csv` (World Inequality Database přes Our World in Data) |
| Giniho koeficient disponibilních příjmů, srovnání zemí | `R/world.R` | `wid_gini.csv` (hromadné stažení WID, 900 MB, uloží se jen vybrané země, EU a svět) |
| Čisté jmění domácností podle kvintilů (HFCS) | `R/czso.R` | `czso_hfcs_jmeni_kvintily.csv` (tab. 12 publikace Finanční situace domácností, Excel) |
| Pacienti ambulantní psychiatrie | `R/uzis.R` | `uzis_ambulantni_psychiatrie.csv` (ÚZIS, NZIP) |
| Srovnání (ne)zaměstnanosti v EU | `R/eurostat.R` | `eurostat_une_rt_a.csv`, `eurostat_lfsi_emp_a.csv` |
| Průměrná a mediánová mzda | `R/czso.R` | `czso_ZAMG08.csv` |
| Mediánová mzda podle krajů | `R/czso.R` | `czso_WREG0404.csv` |
| Distribuce mezd | — | TODO: no machine-readable source found yet |
| Výdaje a příjmy vládního sektoru (% HDP), srovnání zemí | `R/eurostat.R`, `R/world.R` | `eurostat_gov_10a_main.csv` (všechny země); USA, Spojené království a svět z `imf_datamapper.csv` (MMF Fiscal Monitor; svět = průměr vážený HDP v USD, `NGDPD`) |
| Příjmy, výdaje a schodek státního rozpočtu, kategorie příjmů a výdajů | `R/monitor.R` | `monitor_sr.csv` (Monitor státní pokladny, 2010+, jen kapitoly státního rozpočtu, součty podle seskupení položek a skupin paragrafů) |
| Saldo státního rozpočtu (mld. Kč, % HDP) | `R/monitor.R`, `R/eurostat.R` | `monitor_sr.csv`; nominální HDP z `eurostat_gov_10dd_edpt1_cz.csv` (notifikace EDP, `B1GQ`) |
| Dluh vládního sektoru (Kč, % HDP), projekce, srovnání zemí | `R/eurostat.R`, `R/world.R` | `eurostat_gov_10dd_edpt1.csv`; projekce `imf_weo_cze_dluh.csv` (MMF WEO, SDMX API); země mimo EU a Norsko z `imf_datamapper.csv` (`GGXWDG_NGDP`) |
| Výběr daní podle druhu, skladba daní ve srovnání zemí | `R/world.R` | `oecd_revenue_statistics.csv` (OECD Global Revenue Statistics, SDMX API; vybrané druhy daní, Česko, srovnávané země a průměr OECD) |
| Inflace před rokem 1997 (ceny roku 2020 u daní) | `R/world.R` | `wdi_inflace_cz.csv` (Světová banka, FP.CPI.TOTL.ZG); od 1997 `czso_CEN0101H.csv` |

Notes:
- `czso2` workarounds: `czso_download_dataset()` currently saves metadata instead of data, so full datasets are read straight from `data.csu.gov.cz/opendata/sady/{ID}/distribuce/csv`; `czso_get_table()` is only reliable with `format = "CSV"`.
- Employment by national accounts and the productivity Excel are not in DataStat; `R/czso.R` takes them from the national accounts database (`apl.czso.cz/pll/rocenka`). Needs the `readxl` package.
- `czso_produktivita_prace.csv` (ČSÚ's "Ukazatele produktivity práce" Excel) is updated less often than the national accounts: the file from July 2025 gives 537 Kč per hour for 2024, the current accounts give 586 Kč. It uses gross value added, not GDP, and its "na hlavu" series is per inhabitant, not per worker. Prefer computing productivity from GDP and the `czso_nu_*` tables.
- The World Bank API is slow: `R/world.R` can take several minutes.
- Maddison ends in 2022 and is in 2011 international dollars; Czechia starts in 1970, Czechoslovakia in 1820.
- Median wages exist only from 2011 (by region from 2015).
- Monitor (`statnipokladna` package): the table up to 2014 (`budget-central-old`) is cumulative and also contains state funds (kapitoly 05xx); the table from 2015 (`budget-central`) has monthly values that are summed. `R/monitor.R` keeps only kapitoly 03xx (the state budget); the yearly balance then matches the official state budget result (e.g. −271.4 bn Kč in 2024). The download takes a few minutes.

## Website
The site is built by Jekyll; GitHub Pages does that on every push (Settings → Pages → "Deploy from a branch"), so nothing is built or committed locally.

| File | What it is |
|---|---|
| `_config.yml` | site title and tagline, folders left out of the site, default layouts |
| `_layouts/default.html` | the shared `<head>`, header and footer of every page |
| `_layouts/article.html` | title, lede and breadcrumb of an article (wraps `default`) |
| `_includes/sidenav.html`, `assets/nav.js` | side navigation of an article: all articles by topic, plus the `##` and `###` headings of the open one |
| `index.html` | home page: lists everything in `articles/`, grouped by `topic` |
| `articles/*.md` | the articles |

A new article is one Markdown file in `articles/`; it gets its layout and its place on the home page automatically:

```markdown
---
title: Mzdy a zaměstnanost
topic: Ekonomika
order: 2
lede: One sentence shown under the title and on the home page.
---

Text in Markdown.

<figure data-chart="nezamestnanost"></figure>
```

In layouts, link to files with `{{ '/assets/site.css' | relative_url }}` so that the links also work under `/ceskovdatech/` on GitHub Pages.

Preview locally with `jekyll serve` in the project root (install once with `sudo apt install jekyll`), then open `http://localhost:4000`. Opening the files directly or with `python3 -m http.server` no longer works, because the pages need their layout.

## Charts
`write_chart()` in `R/utils.R` writes `chart_data/{id}.json` + `.csv`; `assets/charts.js` (Observable Plot + d3 from a CDN, no build step) turns them into chart cards. Anywhere in a page or article:

```html
<figure data-chart="hdp-realne"></figure>
```

Chart types and their aesthetics (`labeller` = column shown to the reader instead of `field`):

| `type` | required `aes` | optional `aes` |
|---|---|---|
| `line` | `x`, `y` | `colour`, `linetype`, `highlight` |
| `bar` | `x` (category), `y` | `fill` (or `colour`), `highlight` |
| `map` | `geometry` (region code), `fill` | `highlight` |
| `pyramid` | `x` (age), `y` (count), `fill` (the two sides, first value on the left), `frame` (year) | `linetype` |

A `pyramid` shows one value of `frame` at a time, chosen with a slider. It starts at `options$frame`, otherwise at the last frame of the first `linetype` value (the last actual year before a projection). Rows whose `linetype` is not the first value are drawn pale and the value is written next to the year, e.g. "2050 · Projekce ČSÚ". `options$reference` draws that frame as an outline over the others, to compare with.

In a line chart with tabs, each tab draws only the rows that have its `y`, so tabs can also use different rows of the CSV (e.g. one `colour` column per tab, left empty in the other tab's rows).

A projection on a line chart is a `linetype` (e.g. `Skutečnost` / `Projekce ČSÚ`): repeat the last actual year in the projection rows so that the dashed line starts where the solid one ends.

Each aesthetic can have a `format` and a `unit`. The unit is shown on the axis ticks and in tooltips (`1 000 mld. Kč`).

| `format` | 2005 is shown as | note |
|---|---|---|
| `number` | 2 005 | default for numeric columns |
| `year` | 2005 | on `x` of a line or bar chart it also adds the year slider |
| `percent` | 200 500 % | for shares (0.25 → 25 %); the unit defaults to `%` |

`highlight` points to a TRUE/FALSE column and makes the TRUE rows stand out. It is never shown in a legend or tooltip and needs no label. How it looks depends on what is free:

| chart | no `colour`/`fill` | with `colour`/`fill` |
|---|---|---|
| `line` | highlighted line in colour, the rest grey | highlighted line thicker, the rest thin and paler |
| `bar` | highlighted bar in colour, the rest grey | bold category label (also in facet panels) |
| `map` | — | dark outline around highlighted regions |

For a line chart without `colour`, lines are told apart by `linetype` and `highlight` only, so several countries need a `colour` aesthetic too.

An aesthetic with `show = FALSE` is still drawn, but gets no legend (for `x`/`y` no axis title) and no row in the data tooltip, e.g. a `linetype` that only tells two sources apart.

In `title` and `subtitle`, and in the text of an article, `[word::tooltip]` underlines the word and shows the tooltip on hover, focus or tap: `"HDP v [paritě kupní síly::Přepočet podle cenových hladin]"`. Elsewhere (axis labels, data note, PNG) only the word is kept.

`source` is a list of sources, each `list(name, dataset, url, retrieved)`. Sources with the same `name` are shown together, one numbered link per dataset: "Zdroje: ČSÚ (1, 2), Maddison". `data_note` is shown in a tooltip next to the sources.

`tabs` puts a switch into the chart that swaps aesthetics, e.g. between two measures. It is a list of `list(id, label)`; an aesthetic with `tab = "id"` is drawn only in that tab, one without `tab` in every tab. All tabs share `x` (line, bar) or `geometry` (map), so that aesthetic cannot have a `tab`. The first tab is shown first, and the table and CSV keep all columns.

```r
tabs = list(list(id = "realna", label = "Reálná"), list(id = "nominalni", label = "Nominální")),
aes = list(
    list(type = "x", field = "year", label = "Rok", format = "year"),
    list(type = "y", field = "mzda_realna", label = "Reálná mzda", unit = "Kč", tab = "realna"),
    list(type = "y", field = "mzda_nominalni", label = "Nominální mzda", unit = "Kč", tab = "nominalni")
)
```

`options = list(...)` in `write_chart()`, all optional:

| option | applies to | meaning |
|---|---|---|
| `y_zero` | line | start the y axis at zero (default `TRUE`) |
| `y_log` | line | logarithmic y axis |
| `y_domain` | line, bar | fixed axis range, e.g. `c(0, 10)`; bars are cut off at its lower end (an axis that does not start at zero) |
| `highlight` | line, bar | series drawn in colour, the rest in grey |
| `horizontal` | bar | horizontal bars (default: when there are more than 6 text categories) |
| `stack` | bar | `FALSE` puts `fill` groups side by side instead of stacking |
| `sort` | bar | `"desc"` or `"asc"` by value |
| `sort_last` | bar | categories kept at the end of a sorted chart (and of each facet panel), e.g. `"Ostatní"` |
| `timeline` | line, bar | `FALSE` hides the year slider |
| `frame`, `reference` | pyramid | first frame shown; a frame drawn as an outline over the others |
| `filter_latest` | all | name of a time column; only its newest period is drawn |
| `scheme`, `reverse` | map | colour scheme (default `"viridis"`; any Observable Plot scheme, e.g. `"blues"`, or a list of colours, e.g. `c("#d6453d", "#ffffff", "#2e9a4f")`) and its direction |
| `midpoint` | map | two-sided colour scale around this value, e.g. `0` for a change (default scheme `"rdylgn"`) |
| `fill_domain` | map | fixed range of the colour scale, e.g. `c(-0.15, 0.15)`; values outside get the end colours |
| `geo`, `geo_key` | map | file in `geo/` (default `kraje.geojson`) and the feature property matched to `geometry` (default `NUTS_ID`) |
| `projection` | map | default `"mercator"` |
| `facet` | line, bar | `TRUE` adds a toggle that splits the chart into one small chart per series (by `colour` on a line chart, by `fill` on a bar chart), three across; each panel's title shows its latest value. Bars over categories (e.g. countries) become horizontal bars in each panel, sorted highest first |
| `facet_columns` | line, bar | at most this many panels across (default 3) |
| `facet_scale` | line, bar | `"independent"` (default): each panel its own y axis, to show each series' trend; `"shared"`: every panel on the same axis, so sizes can be compared |
| `height` | all | chart height in px |
| `annotations` | line, bar | reference lines, see below |

On a map, `scheme`, `reverse`, `midpoint` and `fill_domain` can also be set on a `fill` aesthetic, where they override `options`. That gives each tab its own colour scale, e.g. `list(type = "fill", field = "exekuce", tab = "exekuce", scheme = "oranges")`.

`annotations` draws dashed reference lines with a label along them: `list(x = ...)` is a vertical line at a value of `x` (for bars, one of the categories), `list(y = ...)` a horizontal line at a value of `y`. A vertical line has vertical text, a horizontal one horizontal text above its right end. `dash = FALSE` makes the line solid; `label` is optional. `align` = `"top"` (default), `"centre"` or `"bottom"` places the label along a vertical line; for a horizontal line it puts the label above, on or below the line. Lines outside the years chosen with the slider are not drawn. With `tab = "..."` a line is drawn only in that tab.

```r
options = list(annotations = list(
    list(x = 2008, label = "Finanční krize"),
    list(x = 2020, label = "Covid"),
    list(y = 0.05, label = "Cíl", dash = FALSE, align = "bottom")
))
```

`Rscript R/geo.R` downloads the kraje boundaries (Eurostat GISCO) into `geo/kraje.geojson`; its `NUTS_ID` equals the ČSÚ kraj code (`CZ010`, …). It also writes `geo/orp.geojson` (správní obvody ORP from the `RCzechia` package, simplified; needs `sf`), used with `options = list(geo = "orp.geojson", geo_key = "KOD_ORP")`; `KOD_ORP` equals the ČSÚ ORP code, Praha is `1000`.

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









