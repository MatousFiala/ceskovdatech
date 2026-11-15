// Renders every <figure data-chart="id"> from chart_data/{id}.json + its CSV.
// The JSON is written by write_chart() in R/utils.R.
import * as Plot from "https://cdn.jsdelivr.net/npm/@observablehq/plot@0.6.17/+esm";
import * as d3 from "https://cdn.jsdelivr.net/npm/d3@7.9.0/+esm";

const ROOT = new URL("..", import.meta.url); // site root, wherever the page lives
const PALETTE = ["#2a78d6", "#e34948", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#eb6834"];
const MUTED = "#c9c8c2";
const DASHES = [null, "6 4", "2 3", "10 4 2 4"];
const FONT = '"DM Sans", "Helvetica Neue", Helvetica, Arial, sans-serif';

// ---------- formatting (Czech) ----------
// Every aesthetic may carry a `format` (see write_chart() in R/utils.R):
//   number  2005 -> "2 005" (default for numeric columns)
//   year    2005 -> "2005"
//   percent 0.25 -> "25 %"  (a share; the unit defaults to "%")
const NBSP = "\u00a0";
const number = (v, digits) => v.toLocaleString("cs-CZ", { maximumFractionDigits: digits });
const date = (v) => new Date(v).toLocaleDateString("cs-CZ");
const FORMATS = {
  // ticks keep their precision, values are rounded to about three significant digits
  number: (v, tick) => number(v, tick ? 3 : Math.abs(v) >= 100 ? 0 : Math.abs(v) >= 10 ? 1 : 2),
  year: (v) => (Number.isInteger(v) ? String(v) : ""),
  percent: (v, tick) => FORMATS.number(v * 100, tick),
};
const unitOf = (a) => a?.unit || (a?.format === "percent" ? "%" : null);

// Formats value `v` of aesthetic `a`; `unit` appends the unit ("1 000 mld. Kč"), `tick` is for axis ticks
function fmt(a, v, { unit = false, tick = false } = {}) {
  if (v == null || v === "") return "–";
  if (v instanceof Date) return date(v);
  if (typeof v !== "number") return String(v);
  const text = (FORMATS[a?.format] ?? FORMATS.number)(v, tick);
  return unit && text && unitOf(a) ? text + NBSP + unitOf(a) : text;
}
const tickOf = (a) => (v) => fmt(a, v, { unit: true, tick: true });
const withUnit = (a) => (unitOf(a) ? `${a.label} (${unitOf(a)})` : a.label);
// Columns no aesthetic describes are shown as they are
const raw = (v) => (v == null || v === "" ? "–" : v instanceof Date ? date(v) : String(v));

// Left margin that fits the widest tick label of a value axis
const measure = document.createElement("canvas").getContext("2d");
function axisMargin(a, values, o, zero = true) {
  let [lo, hi] = o.y_domain ?? d3.extent(values);
  if (lo == null) return 48;
  if (!o.y_domain && !o.y_log && zero) [lo, hi] = [Math.min(0, lo), Math.max(0, hi)];
  const ticks = (o.y_log ? d3.scaleLog() : d3.scaleLinear()).domain([lo, hi]).nice().ticks();
  measure.font = `13px ${FONT}`;
  return Math.ceil(d3.max(ticks, (t) => measure.measureText(tickOf(a)(t)).width) ?? 34) + 14;
}

// ---------- small DOM helper ----------
function h(tag, attrs = {}, ...children) {
  const el = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (k.startsWith("on")) el.addEventListener(k.slice(2), v);
    else if (v === true) el.setAttribute(k, "");
    else if (v != null && v !== false) el.setAttribute(k, v);
  }
  el.append(...children.flat().filter((c) => c != null && c !== false));
  return el;
}

// ---------- inline tooltips: "HDP v [paritě kupní síly::Přepočet podle cenových hladin]" ----------
// "::" or "|" separates the word from its tooltip; Markdown needs "::", since a "|" there starts a table
const TERM = /\[([^\[\]|]+?)(?:\||::)([^\[\]]+)\]/g;
const plain = (text) => String(text ?? "").replace(TERM, "$1"); // where a tooltip cannot be shown (SVG, PNG)
let terms = 0;

// Text with its [word|tooltip] parts turned into underlined words that show the tooltip
function rich(text) {
  const parts = String(text ?? "").split(TERM); // [text, word, tooltip, text, word, tooltip, ...]
  return parts.map((part, i) => {
    if (i % 3 === 0) return part;
    if (i % 3 === 2) return null; // the tooltip, used together with its word
    const id = `chart-term-${++terms}`;
    const tip = h("span", { class: "chart-term-tip", role: "tooltip", id }, parts[i + 1]);
    const place = () => { // keep the tooltip inside the chart card (or, in article text, the text column)
      tip.style.left = "0";
      const card = (term.closest(".chart-card") ?? term.parentElement)?.getBoundingClientRect();
      if (!card) return;
      const box = tip.getBoundingClientRect();
      const shift = Math.max(card.left - box.left, Math.min(0, card.right - box.right));
      tip.style.left = `${shift}px`;
    };
    const term = h("span", {
      class: "chart-term", tabindex: 0, "aria-describedby": id,
      onmouseenter: place, onfocus: place,
      onkeydown: (e) => e.key === "Escape" && term.blur(),
    }, part, tip);
    return term;
  });
}

// ---------- shared plot pieces ----------
const shown = (a) => a && a.show !== false; // aes with show = false: no legend, no axis title, no tooltip row
const label = (a) => a.labeller ?? a.field; // column shown to the reader
const base = (o, width) => ({
  width,
  height: o.height,
  style: { fontFamily: FONT, fontSize: "13px", overflow: "visible" },
});

// Tooltip text: one line per aesthetic, e.g. "Rok: 2020"
function tipText(aes) {
  const seen = new Set();
  const rows = aes.filter((a) => shown(a) && !seen.has(label(a)) && seen.add(label(a)));
  return (d) => rows.map((a) => `${a.label}: ${fmt(a, d[label(a)], { unit: true })}`).join("\n");
}

// The `highlight` aesthetic marks rows to emphasise (TRUE/FALSE or 1/0 column). How they stand out depends on
// what is free: by colour (the rest goes grey), or, when colour already means something, by weight:
// thicker lines, undimmed bars, outlined regions. Returns null when the chart has no highlight.
const isOn = (v) => v === true || v === 1 || /^(true|1)$/i.test(String(v));
const highlighter = (A) => (A.highlight ? (d) => isOn(d?.[A.highlight.field]) : null);

// Colour scale for a categorical column, with optional highlighting
function colourScale(data, a, o) {
  const field = label(a);
  const all = [...new Set(data.map((d) => d[field]))];
  const domain = o.highlight ? all.filter((v) => [o.highlight].flat().includes(v)) : all;
  return { domain, range: PALETTE, unknown: MUTED, legend: all.length > 1 && shown(a) };
}

// ---------- chart types ----------
// `data` is what the year slider leaves, `all` the whole dataset (keeps colours and dashes stable)
function lineChart(spec, data, A, o, width, all) {
  const x = A.x.field, y = A.y.field;
  const colour = A.colour && label(A.colour);
  const dash = A.linetype && label(A.linetype);
  const dashValues = dash ? [...new Set(all.map((d) => d[dash]))] : [null];
  const hi = highlighter(A);
  const series = (d) => `${colour ? d[colour] : ""}|${dash ? d[dash] : ""}|${hi ? hi(d) : ""}`;
  const multi = new Set(all.map(series)).size > 1;
  const zero = !o.y_log && (o.y_zero ?? true);
  const years = A.x.format === "year" ? d3.extent(data, (d) => d[x]) : null;
  // highlight: in colour among grey lines, or thicker among thin ones when colour is taken
  const stroke = colour ?? (hi ? (d) => (hi(d) ? PALETTE[0] : MUTED) : PALETTE[0]);
  const weight = hi && colour ? { strokeWidth: (d) => (hi(d) ? 3.5 : 1.5), strokeOpacity: (d) => (hi(d) ? 1 : 0.6) } : { strokeWidth: hi ? (d) => (hi(d) ? 2.5 : 1.5) : 2 };
  const pointer = multi ? Plot.pointer : Plot.pointerX;
  if (hi) data = d3.sort(data, hi); // highlighted lines last, on top
  if (o.highlight && colour) {
    const hl = [o.highlight].flat(); // draw highlighted series last, on top
    data = d3.sort(data, (d) => hl.includes(d[colour]));
  }

  return Plot.plot({
    ...base(o, width),
    marginLeft: axisMargin(A.y, data.map((d) => d[y]), o, zero),
    x: {
      label: years || !shown(A.x) ? null : A.x.label, tickFormat: tickOf(A.x),
      // whole years only, however short the chosen range is
      ticks: years ? Math.max(1, Math.min(years[1] - years[0], Math.floor(width / 70))) : undefined,
    },
    y: {
      label: shown(A.y) ? A.y.label : null, grid: true, tickFormat: tickOf(A.y),
      type: o.y_log ? "log" : undefined,
      zero: o.y_log ? undefined : o.y_zero ?? true,
      domain: o.y_domain,
    },
    color: colour ? colourScale(all, A.colour, o) : undefined,
    marks: [
      zero ? Plot.ruleY([0]) : null,
      ...dashValues.map((v, i) =>
        Plot.lineY(dash ? data.filter((d) => d[dash] === v) : data, {
          x, y, z: series, stroke, ...weight,
          strokeDasharray: DASHES[i % DASHES.length],
        })),
      Plot.dot(data, pointer({ x, y, fill: stroke, r: 4, stroke: "white" })),
      Plot.tip(data, pointer({ x, y, title: tipText(spec.aes) })),
    ],
  });
}

function barChart(spec, data, A, o, width, all) {
  const cat = label(A.x), y = A.y.field;
  const fillA = A.fill ?? A.colour;
  const hi = highlighter(A);
  // highlight: in colour among grey bars, or at full strength among dimmed ones when colour is taken
  const fill = fillA ? label(fillA) : hi ? (d) => (hi(d) ? PALETTE[0] : MUTED) : PALETTE[0];
  const cats = [...new Set(data.map((d) => d[cat]))];
  const textual = typeof cats[0] === "string";
  const horizontal = o.horizontal ?? (textual && cats.length > 6);
  const grouped = fillA && o.stack === false;

  let domain = cats;
  if (o.sort) {
    const total = d3.rollup(data, (v) => d3.sum(v, (d) => d[y]), (d) => d[cat]);
    domain = d3.sort(cats, (c) => (o.sort === "asc" ? total.get(c) : -total.get(c)));
  }
  const every = Math.ceil(cats.length / 12); // thin out crowded labels (e.g. years)
  const catScale = {
    label: null, domain,
    tickFormat: (d, i) => (textual || i % every === 0 ? fmt(A.x, d, { tick: true }) : ""),
  };
  const valScale = { label: shown(A.y) ? A.y.label : null, grid: true, tickFormat: tickOf(A.y), domain: o.y_domain };
  // stacked bars reach the sum of their category
  const tops = grouped ? data.map((d) => d[y]) : d3.rollups(data, (v) => d3.sum(v, (d) => d[y]), (d) => d[cat]).map((r) => r[1]);
  const longest = d3.max(cats, (c) => String(c).length);
  const left = horizontal ? Math.min(220, longest * 7.5 + 12) : null;
  const title = tipText(spec.aes);
  const mark = horizontal ? Plot.barX : Plot.barY;
  const [catCh, valCh, facetCh] = horizontal ? ["y", "x", "fy"] : ["x", "y", "fx"];

  return Plot.plot({
    ...base(o, width),
    height: o.height ?? (horizontal ? domain.length * (grouped ? 44 : 26) + 50 : undefined),
    marginLeft: left ?? axisMargin(A.y, tops, o),
    marginBottom: horizontal ? 48 : undefined,
    [catCh]: grouped ? { axis: null } : catScale,
    [facetCh]: grouped ? catScale : undefined,
    // under horizontal bars the tick labels sit side by side: only as many as fit
    [valCh]: horizontal ? { ...valScale, ticks: Math.max(2, Math.floor((width - left) / (axisMargin(A.y, tops, o) + 10))) } : valScale,
    color: fillA ? colourScale(all, fillA, o) : undefined,
    marks: [
      mark(data, {
        [grouped ? facetCh : catCh]: cat,
        ...(grouped ? { [catCh]: fill } : {}),
        [valCh]: y, fill, title, tip: true,
        ...(hi && fillA ? { fillOpacity: (d) => (hi(d) ? 1 : 0.4) } : {}),
      }),
      horizontal ? Plot.ruleX([0]) : Plot.ruleY([0]),
    ],
  });
}

const geoCache = new Map();
function loadGeo(file) {
  if (!geoCache.has(file)) {
    geoCache.set(file, d3.json(new URL(`geo/${file}`, ROOT)).then((fc) => {
      // d3 wants clockwise rings; standard GeoJSON is the other way round
      for (const f of fc.features) {
        if (d3.geoArea(f) <= 2 * Math.PI) continue;
        const g = f.geometry;
        (g.type === "Polygon" ? [g.coordinates] : g.coordinates).forEach((p) => p.forEach((r) => r.reverse()));
      }
      return fc;
    }));
  }
  return geoCache.get(file);
}

async function mapChart(spec, data, A, o, width) {
  const fc = await loadGeo(o.geo ?? "kraje.geojson");
  const key = o.geo_key ?? "NUTS_ID";
  const rows = new Map(data.map((d) => [String(d[A.geometry.field]), d]));
  const row = (f) => rows.get(String(f.properties[key]));
  const title = tipText(spec.aes);
  const hi = highlighter(A);
  width = Math.min(width, 760);
  const [, [, height]] = d3.geoPath(d3.geoMercator().fitWidth(width, fc)).bounds(fc);

  const plot = Plot.plot({
    ...base(o, width),
    height: o.height ?? Math.ceil(height),
    projection: { type: o.projection ?? "mercator", domain: fc },
    color: { type: "linear", scheme: o.scheme ?? "viridis", reverse: o.reverse, unknown: "#eeede8" },
    marks: [
      Plot.geo(fc.features, { fill: (f) => row(f)?.[A.fill.field], stroke: "white", strokeWidth: 1 }),
      // highlight: colour is the data here, so highlighted regions get a dark outline
      // (white under dark, so that the outline shows on both ends of the colour scale)
      hi ? Plot.geo(fc.features.filter((f) => hi(row(f))), { fill: "none", stroke: "white", strokeWidth: 5 }) : null,
      hi ? Plot.geo(fc.features.filter((f) => hi(row(f))), { fill: "none", stroke: "#1f1f1e", strokeWidth: 2.5 }) : null,
      // tooltip of the region whose centre is nearest to the pointer
      Plot.tip(fc.features, Plot.pointer(Plot.centroid({
        title: (f) => (row(f) ? title(row(f)) : "Bez dat"),
        maxRadius: 100,
      }))),
    ],
  });
  if (!shown(A.fill)) return plot;

  // Colour legend, drawn separately so that it is wide enough and has only as many ticks as fit
  const tickWidth = axisMargin(A.fill, data.map((d) => d[A.fill.field]), o, false);
  const legendWidth = Math.min(width, 360);
  const legend = plot.legend("color", {
    label: withUnit(A.fill), width: legendWidth, height: 54, marginLeft: 20, marginRight: 20,
    ticks: Math.max(2, Math.floor((legendWidth - 40) / (tickWidth + 12))),
    tickFormat: (v) => fmt(A.fill, v, { tick: true }),
    style: { fontFamily: FONT, fontSize: "13px" },
  });
  return h("div", {}, legend, plot); // the map stays the last svg (see downloadPng)
}

const TYPES = { line: lineChart, bar: barChart, map: mapChart };

// ---------- card: title, tabs, table, footer ----------
function dashLegend(data, A) {
  if (!shown(A.linetype)) return null;
  const values = [...new Set(data.map((d) => d[label(A.linetype)]))];
  return h("div", { class: "chart-legend" }, values.map((v, i) => {
    const item = h("span", { class: "chart-legend-item" });
    item.innerHTML = `<svg width="28" height="10"><line x1="0" x2="28" y1="5" y2="5" stroke="currentColor" stroke-width="2" stroke-dasharray="${DASHES[i % DASHES.length] ?? ""}"/></svg>`;
    item.append(String(v));
    return item;
  }));
}

function dataTable(spec, data) {
  const cols = data.columns;
  const aes = (c) => spec.aes.find((a) => a.field === c);
  const head = (c) => (aes(c) ? withUnit(aes(c)) : spec.aes.find((a) => a.labeller === c)?.label ?? c);
  const cell = (c, v) => (aes(c) ? fmt(aes(c), v) : raw(v));
  return h("table", { class: "chart-table" },
    h("thead", {}, h("tr", {}, cols.map((c) => h("th", {}, head(c))))),
    h("tbody", {}, data.map((d) =>
      h("tr", {}, cols.map((c) => h("td", { class: typeof d[c] === "number" ? "num" : null }, cell(c, d[c])))))));
}

// Sources grouped by publisher: "ČSÚ (1, 2), Maddison Project". Each link opens one dataset.
function sourceGroups(spec) {
  const sources = [spec.source ?? []].flat().filter((s) => s?.name);
  return d3.groups(sources, (s) => s.name);
}

function sourceLine(spec) {
  const link = (s, text) =>
    s.url ? h("a", { href: s.url, title: s.dataset, target: "_blank", rel: "noopener" }, text) : h("span", { title: s.dataset }, text);
  const groups = sourceGroups(spec);
  const parts = groups.flatMap(([name, list], i) => [
    i ? ", " : "",
    ...(list.length === 1
      ? [link(list[0], name)]
      : [`${name} (`, ...list.flatMap((s, j) => [j ? ", " : "", link(s, String(j + 1))]), ")"]),
  ]);
  return h("div", { class: "chart-source" },
    groups.length ? [groups.length > 1 || groups[0][1].length > 1 ? "Zdroje: " : "Zdroj: ", ...parts] : null,
    spec.last_updated ? ` · Aktualizováno ${date(spec.last_updated)}` : null,
    spec.data_note ? [" · ", dataNote(spec)] : null);
}

// Data note: hidden until its button is hovered, focused or tapped (styled in site.css)
function dataNote(spec) {
  const id = `chart-note-${spec.id}`;
  const button = h("button", {
    class: "chart-note-button", type: "button", "aria-describedby": id,
    onclick: () => button.focus(), // Safari does not focus buttons on click
    onkeydown: (e) => e.key === "Escape" && button.blur(),
  }, "ⓘ Poznámka k datům");
  return h("span", { class: "chart-note" }, button, h("span", { class: "chart-note-tip", role: "tooltip", id }, plain(spec.data_note)));
}

// Year slider with two handles; calls onChange(firstYear, lastYear). `gap` = fewest steps between the handles.
function yearSlider(years, gap, onChange) {
  const last = years.length - 1;
  const input = (name, at) => h("input", { type: "range", min: 0, max: last, step: 1, value: at, "aria-label": name });
  const from = input("Od roku", 0), to = input("Do roku", last);
  const fromLabel = h("span", { class: "timeline-year" }), toLabel = h("span", { class: "timeline-year" });
  const track = h("div", { class: "timeline-track" }, h("div", { class: "timeline-fill" }), from, to);
  const sync = (moved) => {
    // the handles cannot cross: the one being dragged stops next to the other
    if (moved === from) from.value = Math.min(+from.value, +to.value - gap);
    else to.value = Math.max(+to.value, +from.value + gap);
    const i0 = +from.value, i1 = +to.value;
    fromLabel.textContent = years[i0];
    toLabel.textContent = years[i1];
    track.style.setProperty("--from", i0 / last);
    track.style.setProperty("--to", i1 / last);
    from.style.zIndex = i0 > last / 2 ? 3 : 1; // keeps a handle reachable when both sit at one end
    if (moved) onChange(years[i0], years[i1]);
  };
  for (const el of [from, to]) el.addEventListener("input", () => sync(el));
  sync();
  return h("div", { class: "chart-timeline" }, fromLabel, track, toLabel);
}

// PNG = title + chart + source drawn on a white canvas (colour legend is not included)
async function downloadPng(spec, body) {
  const live = [...body.querySelectorAll("svg")].at(-1); // the chart itself; legends come before it
  const svg = live.cloneNode(true);
  const { width, height } = live.getBoundingClientRect();
  svg.setAttribute("xmlns", "http://www.w3.org/2000/svg");
  svg.setAttribute("width", width);
  svg.setAttribute("height", height);
  svg.style.color = "#1f1f1e";
  const img = new Image();
  img.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(new XMLSerializer().serializeToString(svg));
  await img.decode();

  const pad = 20, top = 64, bottom = 34, scale = 2;
  const canvas = h("canvas", { width: (width + 2 * pad) * scale, height: (height + top + bottom) * scale });
  const ctx = canvas.getContext("2d");
  ctx.scale(scale, scale);
  ctx.fillStyle = "white";
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  ctx.fillStyle = "#1f1f1e";
  ctx.font = `600 19px ${FONT}`;
  ctx.fillText(plain(spec.title), pad, 30);
  ctx.fillStyle = "#52514e";
  ctx.font = `13px ${FONT}`;
  ctx.fillText(plain(spec.subtitle), pad, 50);
  ctx.drawImage(img, pad, top, width, height);
  ctx.fillStyle = "#77756f";
  ctx.font = `12px ${FONT}`;
  const names = sourceGroups(spec).map(([name]) => name).join(", ");
  ctx.fillText(`Zdroj: ${names} · ceskovcislech`, pad, top + height + 20);
  h("a", { href: canvas.toDataURL("image/png"), download: `${spec.id}.png` }).click();
}

export async function renderChart(el, id = el.dataset.chart) {
  try {
    const dir = new URL("chart_data/", ROOT);
    const spec = await d3.json(new URL(`${id}.json`, dir));
    const csvUrl = new URL(spec.data ?? `${id}.csv`, dir);
    let data = await d3.csv(csvUrl, d3.autoType);
    const o = spec.options ?? {};
    for (const a of spec.aes) {
      a.label = plain(a.label); // labels go into the SVG: plain text only
      if (a.type === "highlight") a.show = false; // never in a legend or the tooltip
    }
    const A = Object.fromEntries(spec.aes.map((a) => [a.type, a]));
    if (o.filter_latest) { // keep only the newest period of the given column
      const latest = d3.max(data, (d) => d[o.filter_latest]);
      data = Object.assign(data.filter((d) => +d[o.filter_latest] === +latest), { columns: data.columns });
    }
    if (!TYPES[spec.type]) throw new Error(`neznámý typ grafu „${spec.type}“`);

    const body = h("div", { class: "chart-body" });
    const table = h("div", { class: "chart-table-wrap", hidden: true }, dataTable(spec, data));

    // Year slider: any line or bar chart whose x is a year (options.timeline = false turns it off)
    const xf = A.x?.field;
    const years = spec.type !== "map" && A.x?.format === "year" && o.timeline !== false
      ? d3.sort(new Set(data.map((d) => d[xf]).filter(Number.isFinite))) : [];
    let shown = data, stale = false;
    const slider = years.length > 2 && yearSlider(years, spec.type === "line" ? 1 : 0, (a, b) => {
      shown = Object.assign(data.filter((d) => d[xf] >= a && d[xf] <= b), { columns: data.columns });
      stale = true;
      table.replaceChildren(dataTable(spec, shown));
      draw();
    });
    const tabs = ["Graf", "Tabulka"].map((name, i) =>
      h("button", {
        class: "chart-tab", type: "button", "aria-pressed": String(i === 0),
        onclick: () => {
          tabs.forEach((t, j) => t.setAttribute("aria-pressed", String(i === j)));
          body.hidden = i !== 0;
          table.hidden = i !== 1;
        },
      }, name));

    el.classList.add("chart-card");
    el.replaceChildren(...[
      h("div", { class: "chart-head" },
        h("div", {},
          h("h3", { class: "chart-title" }, rich(spec.title)),
          spec.subtitle ? h("p", { class: "chart-subtitle" }, rich(spec.subtitle)) : null),
        h("div", { class: "chart-tabs" }, tabs)),
      body,
      table,
      slider,
      h("div", { class: "chart-footer" },
        sourceLine(spec),
        h("div", { class: "chart-actions" },
          h("a", { class: "chart-action", href: csvUrl, download: `${id}.csv` }, "Stáhnout data (CSV)"),
          h("button", { class: "chart-action", type: "button", onclick: () => downloadPng(spec, body) }, "Stáhnout PNG"))),
    ].filter(Boolean));

    let drawn = 0;
    async function draw() { // on first show, on resize and when the year range changes
      const width = Math.round(body.clientWidth);
      if (!width || (width === drawn && !stale)) return; // zero width = the table tab is open
      drawn = width;
      stale = false;
      const plot = await TYPES[spec.type](spec, shown, A, o, width, data);
      body.replaceChildren(...[dashLegend(data, A), plot].filter(Boolean));
    }
    await draw();
    new ResizeObserver(() => requestAnimationFrame(draw)).observe(body);
  } catch (err) {
    console.error(`Chart ${id}:`, err);
    el.replaceChildren(h("p", { class: "chart-error" }, `Graf „${id}“ se nepodařilo načíst (${err.message}).`));
  }
}

// [word|tooltip] also works in the text of an article
function richText(root) {
  const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT, {
    acceptNode: (node) =>
      node.parentElement.closest("[data-chart], pre, code, script, style") || !/\||::/.test(node.data)
        ? NodeFilter.FILTER_REJECT : NodeFilter.FILTER_ACCEPT,
  });
  const nodes = [];
  while (walker.nextNode()) nodes.push(walker.currentNode);
  for (const node of nodes) {
    const parts = rich(node.data).filter((part) => part != null);
    if (parts.length > 1) node.replaceWith(...parts);
  }
}
const article = document.querySelector("main");
if (article) richText(article);

document.querySelectorAll("[data-chart]").forEach((el) => renderChart(el));
