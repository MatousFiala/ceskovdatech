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
| Produktivita práce (osoba, hodina) v Kč | `R/czso.R` | `czso_WNUC03C.csv` ÷ `czso_nu_M0131a.csv` (zaměstnané osoby), `czso_nu_M0131c.csv` (odpracované hodiny, v tisících); řádky `activity == "_T"` |
| Produktivita práce, hotové ukazatele ČSÚ | `R/czso.R` | `czso_produktivita_prace.csv` |
| Produktivita práce, srovnání s EU (PPS) | `R/eurostat.R` | `eurostat_nama_10_lp_ulc.csv` |
| Srovnání HDP na osobu (PPP) | `R/world.R` | `wdi_gdp_pc_ppp.csv`, `maddison_gdp_pc.csv` |
| Míra (ne)zaměstnanosti, i podle krajů | `R/czso.R` | `czso_ZAM01.csv` |
| Srovnání (ne)zaměstnanosti v EU | `R/eurostat.R` | `eurostat_une_rt_a.csv`, `eurostat_lfsi_emp_a.csv` |
| Průměrná a mediánová mzda | `R/czso.R` | `czso_ZAMG08.csv` |
| Mediánová mzda podle krajů | `R/czso.R` | `czso_WREG0404.csv` |
| Distribuce mezd | — | TODO: no machine-readable source found yet |

Notes:
- `czso2` workarounds: `czso_download_dataset()` currently saves metadata instead of data, so full datasets are read straight from `data.csu.gov.cz/opendata/sady/{ID}/distribuce/csv`; `czso_get_table()` is only reliable with `format = "CSV"`.
- Employment by national accounts and the productivity Excel are not in DataStat; `R/czso.R` takes them from the national accounts database (`apl.czso.cz/pll/rocenka`). Needs the `readxl` package.
- `czso_produktivita_prace.csv` (ČSÚ's "Ukazatele produktivity práce" Excel) is updated less often than the national accounts: the file from July 2025 gives 537 Kč per hour for 2024, the current accounts give 586 Kč. It uses gross value added, not GDP, and its "na hlavu" series is per inhabitant, not per worker. Prefer computing productivity from GDP and the `czso_nu_*` tables.
- The World Bank API is slow: `R/world.R` can take several minutes.
- Maddison ends in 2022 and is in 2011 international dollars; Czechia starts in 1970, Czechoslovakia in 1820.
- Median wages exist only from 2011 (by region from 2015).

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
| `bar` | highlighted bar in colour, the rest grey | the other bars dimmed |
| `map` | — | dark outline around highlighted regions |

For a line chart without `colour`, lines are told apart by `linetype` and `highlight` only, so several countries need a `colour` aesthetic too.

An aesthetic with `show = FALSE` is still drawn, but gets no legend (for `x`/`y` no axis title) and no row in the data tooltip, e.g. a `linetype` that only tells two sources apart.

In `title` and `subtitle`, and in the text of an article, `[word::tooltip]` underlines the word and shows the tooltip on hover, focus or tap: `"HDP v [paritě kupní síly::Přepočet podle cenových hladin]"`. Elsewhere (axis labels, data note, PNG) only the word is kept.

`source` is a list of sources, each `list(name, dataset, url, retrieved)`. Sources with the same `name` are shown together, one numbered link per dataset: "Zdroje: ČSÚ (1, 2), Maddison". `data_note` is shown in a tooltip next to the sources.

`options = list(...)` in `write_chart()`, all optional:

| option | applies to | meaning |
|---|---|---|
| `y_zero` | line | start the y axis at zero (default `TRUE`) |
| `y_log` | line | logarithmic y axis |
| `y_domain` | line, bar | fixed axis range, e.g. `c(0, 10)` |
| `highlight` | line, bar | series drawn in colour, the rest in grey |
| `horizontal` | bar | horizontal bars (default: when there are more than 6 text categories) |
| `stack` | bar | `FALSE` puts `fill` groups side by side instead of stacking |
| `sort` | bar | `"desc"` or `"asc"` by value |
| `timeline` | line, bar | `FALSE` hides the year slider |
| `filter_latest` | all | name of a time column; only its newest period is drawn |
| `scheme`, `reverse` | map | colour scheme (default `"viridis"`; any Observable Plot scheme, e.g. `"blues"`) and its direction |
| `geo`, `geo_key` | map | file in `geo/` (default `kraje.geojson`) and the feature property matched to `geometry` (default `NUTS_ID`) |
| `projection` | map | default `"mercator"` |
| `height` | all | chart height in px |

`Rscript R/geo.R` downloads the kraje boundaries (Eurostat GISCO) into `geo/kraje.geojson`; its `NUTS_ID` equals the ČSÚ kraj code (`CZ010`, …).

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









