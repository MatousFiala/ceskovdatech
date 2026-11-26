# Česko v číslech: technical documentation

How the site is built: data, chart specifications, the website. For what the project is, see the [README](README.md).

- [Article front matter](#article-front-matter)
- [Data](#data)
- [Website](#website)
- [Charts](#charts)

## Article front matter
| Key | Meaning |
|---|---|
| `title` | Title of the article (also on the home page and in the browser tab) |
| `topic` | One of the topics in `topics` in `_config.yml` |
| `order` | Position within the topic |
| `description` | One sentence for search engines and link previews |
| `updated` | Date of the last update, `YYYY-MM-DD`; shown under the title |
| `status: tbd` | Upcoming article: greyed out on the home page without a link, skipped by "next article", the page shows only a notice and is not indexed. Remove the line when the article is ready |

## Data
`Rscript run_all.R` (from the project root) downloads the newest data into `data/`, one CSV per source dataset. Each script in `R/` can also be run on its own. Data is saved raw; derived series (e.g. real GDP per capita) are computed later, at the chart stage.

Needs the R packages `tidyverse`, `czso2`, `eurostat`, `WDI`, `statnipokladna` and `charta` (from `lib/charta/r`, see [Charts](#charts)).

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
| Saldo státního rozpočtu (% HDP) | `R/monitor.R`, `R/eurostat.R` | `monitor_sr.csv`; nominální HDP z `eurostat_gov_10dd_edpt1_cz.csv` (`B1GQ`) |
| Dluh vládního sektoru (Kč, % HDP), projekce, srovnání zemí | `R/eurostat.R`, `R/world.R` | `eurostat_gov_10dd_edpt1.csv`; projekce `imf_weo_cze.csv` (MMF WEO, SDMX API; i inflace `PCPIPCH` pro reálný dluh v letech projekce); země mimo EU a Norsko z `imf_datamapper.csv` (`GGXWDG_NGDP`) |
| Výběr daní podle druhu, skladba daní ve srovnání zemí | `R/world.R` | `oecd_revenue_statistics.csv` (OECD Global Revenue Statistics, SDMX API; vybrané druhy daní, Česko, srovnávané země a průměr OECD) |
| Skladba výdajů vládního sektoru ve srovnání zemí | `R/world.R` | `oecd_cofog.csv` (OECD National Accounts, Table 11: výdaje podle funkcí COFOG, oddíly a celkem), `oecd_hdp.csv` (nominální HDP); všechny členské země OECD (pro průměr OECD) a EU27 |
| Zdanění práce podle výše mzdy, ve srovnání zemí a od roku 2000 | `R/world.R` | `oecd_taxing_wages.csv` (OECD Taxing Wages, rozklad daňového klínu, poslední rok: 50–250 % průměrné mzdy, svobodný bez dětí, země jako daně a průměr OECD), `oecd_taxing_wages_cz.csv` (Česko od 2000, svobodný bez dětí, 67–167 % průměrné mzdy) |
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
| `_config.yml` | site title and description, `topics` (the topics of the home page: one list per column), files and folders left out of the site, default layouts |
| `_layouts/default.html` | the shared `<head>`, header (logo, links to Témata and O projektu) and footer of every page |
| `assets/site.css` | the whole look (light only): colours (CSS variables, also used by the chart cards), layout |
| `lib/charta/` | the chart library (submodule, see [Charts](#charts)): `js/charts.js`, `js/charts.css` and `js/vendor/` with d3 7.9.0 and Observable Plot 0.6.17, loaded as plain scripts in `default.html` before the modules, which read them from `window` |
| `assets/fonts/` | Inter (variable, Latin and Latin Extended), served from this site rather than Google Fonts so that no visitor data goes to a third party; `inter.css` declares it, `OFL.txt` is its licence. The PNG export embeds the same files |
| `assets/img/` | logo: `*-hlavicka-*` (wordmark, used in the header: `svetla` = for a light background) and `*-znacka-*` (the mark alone, also in the PNG export of charts); `favicon.svg` is a square copy of the mark whose outlines turn light in a dark browser theme |
| `_layouts/article.html` | breadcrumb, title, the update line (from `updated`) and the link to the next article (wraps `default`); an article with `status: tbd` shows only a notice |
| `_includes/sidenav.html`, `assets/nav.js` | side navigation of an article: back to the topics, plus the `##` and `###` headings of the article, marking the one being read |
| `index.html` | home page: the ORP map, the topics (everything in `articles/`, by `topic`, in the columns set by `topics` in `_config.yml`, with an icon from `_includes/topic-icon.html`; upcoming articles greyed out) and O projektu |
| `assets/home.js`, `assets/orp-hex.json` | the home-page map: 206 ORP hexes (geometry from the Claude Design file, with Jeseník ↔ Rýmařov and Uherské Hradiště ↔ Veselí nad Moravou swapped so that the kraje are contiguous; ORP codes as in `geo/orp.geojson`; the kraj borders are the edges between hexes whose codes start with different two digits). On the flag, each ORP is shaded by `vyska`, its average height in metres (RCzechia `vyskopis()`, computed once and stored in the file) that turn, with d3 transitions every 9 s, from the flag into three ORP charts (`obyvatele-zmena-orp`, `zdravi-nadeje-orp`, `chudoba-isv-orp`, read from `chart_data/`) |
| `articles/*.md` | the articles |

A new article is one Markdown file in `articles/`; it gets its layout and its place on the home page automatically:

```markdown
---
title: Zaměstnanost a mzdy
topic: Ekonomika
order: 2
description: "One sentence for search engines and link previews."
updated: 2026-11-21
---

Text in Markdown.

<figure data-chart="nezamestnanost-cr"></figure>
```

The site runs on its own domain, `ceskovcislech.org` (`CNAME`, `url` and `domain` in `_config.yml`). Articles get short addresses from their file name: `articles/hdp_rust.md` → `ceskovcislech.org/hdp_rust` (`permalink` in `_config.yml`); exported charts print that address. In layouts, still link to files with `{{ '/assets/site.css' | relative_url }}`.

Preview locally with `jekyll serve` in the project root (install once with `sudo apt install jekyll`), then open `http://localhost:4000`. Opening the files directly or with `python3 -m http.server` no longer works, because the pages need their layout.

## Charts
The charts come from **Charta**, a separate library in `lib/charta` (a git submodule, [github.com/MatousFiala/charta](https://github.com/MatousFiala/charta)). Its [README](lib/charta/README.md) documents chart types, aesthetics, options, tabs, annotations and the PNG export.

- **R:** `write_chart()` comes from the `charta` R package. `R/utils.R` loads it, together with tidyverse and jsonlite, for the scripts in `R/`. `R/geo.R` calls `write_geo()` for the map boundaries in `geo/`.
- **Browser:** `assets/charts.js` is this site's wrapper. It calls `configure()` with the site root (where `chart_data/` and `geo/` live), the logo of the PNG export and the page address, and then `renderAll()`. Anywhere in a page or article:

```html
<figure data-chart="hdp-realne"></figure>
```

After cloning, run `git submodule update --init`. Install the R package from the submodule with `remotes::install_local("lib/charta/r")`, and run that again after you update it. To update Charta, run `git submodule update --remote lib/charta`, then commit the new submodule commit. GitHub Pages checks out the submodule itself.
