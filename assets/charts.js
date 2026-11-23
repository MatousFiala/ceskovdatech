// Renders every <figure data-chart="id"> from chart_data/{id}.json + its CSV.
// The JSON is written by write_chart() in R/utils.R.
// d3 7.9.0 and Observable Plot 0.6.17 are served from this site (assets/vendor/, loaded in _layouts/default.html)
const { d3, Plot } = window;

const ROOT = new URL("..", import.meta.url); // site root, wherever the page lives
// series colours of the site design first (blue and red of the flag, then green, amber, purple, pink)
const PALETTE = ["#2F6DB5", "#D7141A", "#1E9E77", "#E3A008", "#7A5CC8", "#E87BA4", "#008300", "#eb6834", "#8a5a2b", "#5c6670"];
const MUTED = "#c9c5bc";
const INK = "#161616", INK_2 = "#45433E", INK_3 = "#6B675F", BG = "#FBFAF7"; // = site.css --text-*, --bg
const DASHES = [null, "6 4", "2 3", "10 4 2 4"];
const FONT = '"Inter", system-ui, -apple-system, "Segoe UI", Arial, sans-serif';

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
// a long unit ("na 100 000 obyvatel") would crowd every tick: it goes to the axis title instead
const longUnit = (a) => (unitOf(a)?.length ?? 0) > 8;
const tickOf = (a) => (v) => fmt(a, v, { unit: !longUnit(a), tick: true });
const axisLabel = (a) => (longUnit(a) ? `${a.label} (${unitOf(a)})` : a.label);
// a label that already says "%" ("% HDP", "Výdaje (% HDP)") does not get the default "(%)" again
const withUnit = (a) => (unitOf(a) && (a.unit || !String(a.label).includes("%")) ? `${a.label} (${unitOf(a)})` : a.label);
// Columns no aesthetic describes are shown as they are
const raw = (v) => (v == null || v === "" ? "–" : v instanceof Date ? date(v) : String(v));

// Ticks of a horizontal value axis, labels side by side: as many as fit `avail` px (a count would only be a
// hint, and d3 may return more ticks than that)
function fitTicks(a, values, o, avail, zero = true) {
  if (o.y_log) return Math.max(2, Math.floor(avail / (axisMargin(a, values, o) + 10)));
  let [lo, hi] = o.y_domain ?? d3.extent(values);
  if (lo == null) return 2;
  if (!o.y_domain && zero) [lo, hi] = [Math.min(0, lo), Math.max(0, hi)];
  measure.font = `13px ${FONT}`;
  // at least two ticks: a count of 1 can step past the end of the data (0–716 → step 1000) and leave only 0,
  // i.e. no labels and no grid at all
  let fallback = null;
  for (let n = Math.max(2, Math.floor(avail / 60)); n >= 1; n--) {
    const ticks = d3.ticks(lo, hi, n);
    if (ticks.length < 2) continue;
    fallback = ticks;
    const widest = d3.max(ticks, (t) => measure.measureText(tickOf(a)(t)).width) ?? 0;
    if (ticks.length * (widest + 16) <= avail) return ticks;
  }
  return fallback ?? d3.ticks(lo, hi, 2);
}

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
  marginTop: 32, // room for the y-axis label above the chart
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

// Colour scale for a categorical column, with optional highlighting. `hi` (line charts): series whose rows are
// all highlighted come first, so they get the first palette colour
function colourScale(data, a, o, hi) {
  const field = label(a);
  let all = [...new Set(data.map((d) => d[field]))];
  if (hi) {
    const whole = (v) => data.every((d) => d[field] !== v || hi(d));
    all = [...all.filter(whole), ...all.filter((v) => !whole(v))];
  }
  const domain = o.highlight ? all.filter((v) => [o.highlight].flat().includes(v)) : all;
  const range = a.colours ? domain.map((v, i) => seriesColour(a, v, i)) : gradient(o, domain.length) ?? PALETTE;
  return { domain, range, unknown: MUTED, legend: all.length > 1 && shown(a) };
}

// fixed colours of a colour/fill aesthetic (colours = list(Příjmy = "#2F6DB5", …)), else the palette in order
const seriesColour = (a, v, i) => a?.colours?.[v] ?? PALETTE[i % PALETTE.length];

// options.colour_scheme (e.g. "blues"): ordered series in shades of one colour, light to dark in data order;
// the lightest shades are left out, so that every series stays visible on white
function gradient(o, n) {
  if (!o.colour_scheme) return null;
  const name = String(o.colour_scheme);
  const f = d3[`interpolate${name[0].toUpperCase()}${name.slice(1).toLowerCase()}`];
  if (!f) throw new Error(`neznámá barevná škála „${name}“`);
  return d3.range(n).map((i) => f(n > 1 ? 0.55 + (0.45 * i) / (n - 1) : 0.8));
}

// ---------- annotations ----------
// Reference lines from options.annotations, each {x | y, label, dash, align}: `x` is a value of the x aesthetic
// (a category for bars), `y` a value of y. Dashed unless dash = false; the label runs along the line,
// so a vertical line has vertical text. align (top | centre | bottom, default top): where the label sits
// along a vertical line; for a horizontal line, whether it sits above, on or below the line (at its right end). `ch` = which scale x and y values are on (swapped for horizontal bars),
// `xValue` turns an x value into the one in the data, or null when it is not shown (year slider).
const annotationValues = (o) => (o.annotations ?? []).filter((a) => a.y != null).map((a) => +a.y);
function annotationMarks(o, { ch = { x: "x", y: "y" }, xValue = (v) => v } = {}) {
  return (o.annotations ?? []).flatMap((a) => {
    const along = a.x != null ? "x" : "y";
    const v = along === "x" ? xValue(a.x) : +a.y;
    if (v == null || Number.isNaN(v)) return [];
    const channel = ch[along];
    const vertical = channel === "x";
    const rule = (vertical ? Plot.ruleX : Plot.ruleY)([v], {
      stroke: INK_3, strokeWidth: 1, strokeDasharray: a.dash === false ? null : "4 3",
    });
    if (!a.label) return [rule];
    const align = { center: "middle", centre: "middle", middle: "middle", bottom: "bottom" }[a.align] ?? "top";
    const text = Plot.text([{ v, label: plain(a.label) }], {
      [channel]: "v", text: "label",
      fill: INK_2, fontSize: 12, stroke: BG, strokeWidth: 3, paintOrder: "stroke",
      ...(vertical
        ? { // reads upwards, just left of the line
          rotate: -90, lineAnchor: "bottom", dx: -3, frameAnchor: align,
          textAnchor: { top: "end", middle: "middle", bottom: "start" }[align],
          dy: { top: 2, middle: 0, bottom: -2 }[align],
        }
        : { // at the right end of the line
          frameAnchor: "right", textAnchor: "end",
          lineAnchor: { top: "bottom", middle: "middle", bottom: "top" }[align],
          dy: { top: -3, middle: 0, bottom: 3 }[align],
        }),
    });
    return [rule, text];
  });
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
  const weight = hi && colour ? { strokeWidth: (d) => (hi(d) ? 3.5 : 2.5), strokeOpacity: (d) => (hi(d) ? 1 : 0.75) } : { strokeWidth: hi ? (d) => (hi(d) ? 2.5 : 1.5) : 2 };
  const pointer = multi ? Plot.pointer : Plot.pointerX;
  if (hi) data = d3.sort(data, hi); // highlighted lines last, on top
  if (o.highlight && colour) {
    const hl = [o.highlight].flat(); // draw highlighted series last, on top
    data = d3.sort(data, (d) => hl.includes(d[colour]));
  }

  return Plot.plot({
    ...base(o, width),
    marginLeft: axisMargin(A.y, [...data.map((d) => d[y]), ...annotationValues(o)], o, zero),
    x: {
      label: years || !shown(A.x) ? null : axisLabel(A.x), tickFormat: tickOf(A.x),
      // whole years only, however short the chosen range is
      ticks: years ? Math.max(1, Math.min(years[1] - years[0], Math.floor(width / 70))) : undefined,
    },
    y: {
      label: shown(A.y) ? axisLabel(A.y) : null, grid: true, tickFormat: tickOf(A.y),
      type: o.y_log ? "log" : undefined,
      zero: o.y_log ? undefined : o.y_zero ?? true,
      domain: o.y_domain,
    },
    color: colour ? colourScale(all, A.colour, o, hi) : undefined,
    marks: [
      zero ? Plot.ruleY([0]) : null,
      ...dashValues.map((v, i) =>
        Plot.lineY(dash ? data.filter((d) => d[dash] === v) : data, {
          x, y, z: series, stroke, ...weight,
          strokeDasharray: DASHES[i % DASHES.length],
        })),
      ...annotationMarks(o, { xValue: (v) => {
        const [lo, hi] = d3.extent(data, (d) => d[x]);
        if (lo instanceof Date) v = new Date(v);
        return v >= lo && v <= hi ? v : null; // outside the chosen years: not drawn
      } }),
      Plot.dot(data, pointer({ x, y, fill: stroke, r: 4, stroke: BG })),
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
  // text categories lie down when there are many, or when their labels would not fit side by side under the bars
  measure.font = `13px ${FONT}`;
  const crowded = textual && d3.max(cats, (c) => measure.measureText(fmt(A.x, c, { tick: true })).width) > width / cats.length - 12;
  const horizontal = o.horizontal ?? (textual && (cats.length > 6 || crowded));
  const grouped = fillA && o.stack === false;

  let domain = cats;
  if (o.sort) {
    const total = d3.rollup(data, (v) => d3.sum(v, (d) => d[y]), (d) => d[cat]);
    domain = sortLast(d3.sort(cats, (c) => (o.sort === "asc" ? total.get(c) : -total.get(c))), o);
  }
  const every = Math.ceil(cats.length / Math.min(12, Math.max(2, Math.floor(width / 60)))); // thin out crowded labels (e.g. years)
  const catScale = {
    label: null, domain,
    tickFormat: (d, i) => (textual || horizontal || i % every === 0 ? fmt(A.x, d, { tick: true }) : ""),
  };
  const valScale = { label: shown(A.y) ? axisLabel(A.y) : null, grid: true, tickFormat: tickOf(A.y), domain: o.y_domain };
  // stacked bars reach the sum of their category
  const tops = grouped ? data.map((d) => d[y]) : d3.rollups(data, (v) => d3.sum(v, (d) => d[y]), (d) => d[cat]).map((r) => r[1]);
  tops.push(...annotationValues(o));
  const longest = d3.max(cats, (c) => String(c).length);
  const left = horizontal ? Math.min(250, longest * 7.5 + 12) : null;
  const title = tipText(spec.aes);
  const mark = horizontal ? Plot.barX : Plot.barY;
  const [catCh, valCh, facetCh] = horizontal ? ["y", "x", "fy"] : ["x", "y", "fx"];

  const plot = Plot.plot({
    ...base(o, width),
    height: o.height ?? (horizontal ? domain.length * (grouped ? 44 : 26) + 50 : undefined),
    marginLeft: left ?? axisMargin(A.y, tops, o),
    marginBottom: horizontal ? 48 : undefined,
    [catCh]: grouped ? { axis: null } : catScale,
    [facetCh]: grouped ? catScale : undefined,
    // under horizontal bars the tick labels sit side by side: only as many as fit
    [valCh]: horizontal ? { ...valScale, ticks: fitTicks(A.y, tops, o, width - left) } : valScale,
    color: fillA ? colourScale(all, fillA, o) : undefined,
    marks: [
      mark(data, {
        [grouped ? facetCh : catCh]: cat,
        ...(grouped ? { [catCh]: fill } : {}),
        [valCh]: y, fill, title, tip: true,
        clip: o.y_domain ? true : undefined, // a fixed axis range that leaves out zero would let bars spill out
      }),
      horizontal ? Plot.ruleX([0]) : Plot.ruleY([0]),
      ...totalMarks(data, { cat, y, catCh, valCh, on: o.totals && fillA && !grouped }),
      ...annotationMarks(o, {
        ch: { x: catCh, y: valCh },
        // a category that is shown; none for side-by-side groups, whose categories are facets
        xValue: (v) => (grouped ? null : domain.find((c) => String(c) === String(v)) ?? null),
      }),
    ],
  });
  // fill already gives the colours: the highlighted categories get a bold label instead
  if (hi && fillA) boldTicks(plot, data.filter(hi).map((d) => d[cat]));
  return plot;
}

// options.range = c(from, point, to) on a dot chart: three values of the colour column. Each row (or year, when x is
// a number) gets a line from its `from` value to its `to` value with short caps at both ends and a point at `point`;
// highlighted rows in colour, the rest grey. The tooltip lists every colour series of the row.
function rangeChart(spec, data, A, o, width) {
  const cat = label(A.x), y = A.y.field, colour = label(A.colour);
  const [from, mid, to] = o.range.map(String);
  const hi = highlighter(A);
  const vertical = typeof data[0]?.[cat] === "number"; // years along x, values up
  let cats = [...new Set(data.map((d) => d[cat]))];
  if (o.sort_by != null) {
    const v = new Map(data.filter((d) => String(d[colour]) === String(o.sort_by)).map((d) => [d[cat], d[y]]));
    cats = d3.sort(cats, (c) => (o.sort === "desc" ? -v.get(c) : v.get(c)));
  }
  cats = sortLast(cats, o);
  const rows = d3.rollups(data, (v) => {
    const at = (s) => v.find((d) => String(d[colour]) === s)?.[y];
    const tip = [fmt(A.x, v[0][cat]), ...v.map((d) => `${fmt(A.colour, d[colour])}: ${fmt(A.y, d[y], { unit: true })}`)].join("\n");
    return { lo: at(from), mid: at(mid), hi: at(to), on: hi ? v.some(hi) : true, tip };
  }, (d) => d[cat]).map(([c, r]) => ({ c, ...r })).filter((r) => r.lo != null && r.hi != null);
  const stroke = (r) => (r.on ? PALETTE[0] : "#9b9993"); // darker than MUTED: thin lines in light grey are hard to see
  const values = [...rows.flatMap((r) => [r.lo, r.hi]), ...annotationValues(o)];
  const valAxis = { label: shown(A.y) ? axisLabel(A.y) : null, grid: true, tickFormat: tickOf(A.y), domain: o.y_domain, zero: o.y_zero ?? false };
  const key = h("div", { class: "chart-legend" },
    `${A.colour.label}: čára ${fmt(A.colour, from)} až ${fmt(A.colour, to)}, bod ${fmt(A.colour, mid)}`);
  let plot;
  if (vertical) {
    const cap = 0.3; // half-width of the end caps, in years
    const [x0, x1] = d3.extent(rows, (r) => r.c);
    plot = Plot.plot({
      ...base(o, width),
      marginLeft: axisMargin(A.y, values, o, false),
      x: { label: null, tickFormat: tickOf(A.x), ticks: Math.max(2, Math.min(x1 - x0, Math.floor(width / 70))) },
      y: valAxis,
      marks: [
        ...annotationMarks(o, { xValue: (v) => (v >= x0 && v <= x1 ? v : null) }),
        Plot.ruleX(rows, { x: "c", y1: "lo", y2: "hi", stroke, strokeWidth: 2 }),
        Plot.ruleY(rows, { y: "lo", x1: (r) => r.c - cap, x2: (r) => r.c + cap, stroke, strokeWidth: 2 }),
        Plot.ruleY(rows, { y: "hi", x1: (r) => r.c - cap, x2: (r) => r.c + cap, stroke, strokeWidth: 2 }),
        Plot.dot(rows, { x: "c", y: "mid", fill: stroke, r: 4, stroke: BG, strokeWidth: 1 }),
        Plot.tip(rows, Plot.pointerX({ x: "c", y: "mid", title: "tip" })),
      ],
    });
  } else {
    const left = Math.min(250, d3.max(cats, (c) => String(c).length) * 7.5 + 12);
    plot = Plot.plot({
      ...base(o, width),
      height: o.height ?? cats.length * 26 + 50,
      marginLeft: left,
      marginBottom: 48,
      y: { label: null, domain: cats, tickFormat: (d) => fmt(A.x, d, { tick: true }) },
      x: { ...valAxis, ticks: fitTicks(A.y, values, o, width - left, o.y_zero ?? false) },
      marks: [
        ...annotationMarks(o, { ch: { x: "y", y: "x" }, xValue: (v) => cats.find((c) => String(c) === String(v)) ?? null }),
        Plot.ruleY(rows, { y: "c", x1: "lo", x2: "hi", stroke, strokeWidth: 2 }),
        Plot.tickX(rows, { y: "c", x: "lo", stroke, strokeWidth: 2, insetTop: 7, insetBottom: 7 }),
        Plot.tickX(rows, { y: "c", x: "hi", stroke, strokeWidth: 2, insetTop: 7, insetBottom: 7 }),
        Plot.dot(rows, { y: "c", x: "mid", fill: stroke, r: 5, stroke: BG, strokeWidth: 1 }),
        Plot.tip(rows, Plot.pointerY({ y: "c", x: "mid", title: "tip" })),
      ],
    });
    if (hi) boldTicks(plot, rows.filter((r) => r.on).map((r) => r.c));
  }
  return h("div", {}, key, plot);
}

// Dot plot: one row per category (x, in data order, first on top), one point per colour series at its value (y),
// joined by a grey line from the lowest to the highest point of the row
function dotChart(spec, data, A, o, width, all) {
  const cat = label(A.x), y = A.y.field;
  const colour = A.colour && label(A.colour);
  const hi = highlighter(A);
  if (o.range && colour) return rangeChart(spec, data, A, o, width);
  let cats = [...new Set(data.map((d) => d[cat]))];
  // options.sort_by = a colour series: rows ordered by its value (lowest first, or highest with sort = "desc"), in each tab
  if (o.sort_by != null && colour) {
    const v = new Map(data.filter((d) => String(d[colour]) === String(o.sort_by)).map((d) => [d[cat], d[y]]));
    cats = d3.sort(cats, (c) => (o.sort === "desc" ? -v.get(c) : v.get(c)));
  }
  cats = sortLast(cats, o);
  const values = [...data.map((d) => d[y]), ...annotationValues(o)];
  const left = Math.min(250, d3.max(cats, (c) => String(c).length) * 7.5 + 12);
  const rows = d3.rollups(data, (v) => d3.extent(v, (d) => d[y]), (d) => d[cat]).map(([c, [lo, hi]]) => ({ c, lo, hi }));
  const fill = colour ?? (hi ? (d) => (hi(d) ? PALETTE[0] : MUTED) : PALETTE[0]);

  const plot = Plot.plot({
    ...base(o, width),
    height: o.height ?? cats.length * 26 + 50,
    marginLeft: left,
    marginBottom: 48,
    y: { label: null, domain: cats, tickFormat: (d) => fmt(A.x, d, { tick: true }) },
    x: {
      label: shown(A.y) ? axisLabel(A.y) : null, grid: true, tickFormat: tickOf(A.y), domain: o.y_domain,
      zero: o.y_zero ?? false,
      ticks: fitTicks(A.y, values, o, width - left, o.y_zero ?? false),
    },
    color: colour ? colourScale(all, A.colour, o) : undefined,
    marks: [
      Plot.ruleY(rows, { y: "c", x1: "lo", x2: "hi", stroke: "#d6d4cd", strokeWidth: 2 }),
      ...annotationMarks(o, { ch: { x: "y", y: "x" }, xValue: (v) => cats.find((c) => String(c) === String(v)) ?? null }),
      Plot.dot(data, { y: cat, x: y, fill, r: 5, stroke: BG, strokeWidth: 1, title: tipText(spec.aes), tip: true }),
    ],
  });
  // colour already gives the series: the highlighted categories get a bold label instead
  if (hi && colour) boldTicks(plot, data.filter(hi).map((d) => d[cat]));
  return plot;
}

// options.totals on a stacked bar chart: a dark tick at each category's net total (negative parts subtracted)
function totalMarks(data, { cat, y, catCh, valCh, on }) {
  if (!on) return [];
  const rows = d3.rollups(data, (v) => d3.sum(v, (d) => d[y]), (d) => d[cat]).map(([c, net]) => ({ c, net }));
  return [(valCh === "y" ? Plot.tickY : Plot.tickX)(rows, { [catCh]: "c", [valCh]: "net", stroke: INK, strokeWidth: 2.5 })];
}

// Bold tick labels for the given categories (highlight on bar charts whose fill is taken)
function boldTicks(node, cats) {
  const names = new Set(cats.map(String));
  for (const t of node.querySelectorAll('g[aria-label$="tick label"] text')) {
    if (names.has(t.textContent)) t.setAttribute("font-weight", 700);
  }
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
  // colour settings on the fill aesthetic win over options: each tab can have its own scale
  const c = { ...o };
  for (const k of ["scheme", "reverse", "midpoint", "fill_domain"]) if (A.fill[k] != null) c[k] = A.fill[k];
  const diverging = c.midpoint != null;
  width = Math.min(width, 760);
  const [, [, height]] = d3.geoPath(d3.geoMercator().fitWidth(width, fc)).bounds(fc);

  const plot = Plot.plot({
    ...base(o, width),
    height: o.height ?? Math.ceil(height),
    projection: { type: o.projection ?? "mercator", domain: fc },
    color: {
      // midpoint: two-sided scale around that value (e.g. 0 for a change); scheme: a name, or a list of colours
      type: diverging ? "diverging" : "linear", pivot: diverging ? c.midpoint : undefined,
      ...(Array.isArray(c.scheme) ? { interpolate: d3.piecewise(d3.interpolateRgb, c.scheme) }
        : paleStart(c.scheme ?? (diverging ? null : "blues"), diverging) ? { interpolate: paleStart(c.scheme ?? "blues", diverging) }
        : { scheme: c.scheme ?? "rdylgn" }), // default: blues, or red–yellow–green around a midpoint
      domain: c.fill_domain, clamp: c.fill_domain ? true : undefined, // values outside get the end colours
      reverse: c.reverse, unknown: "#eeede8",
    },
    marks: [
      // a two-sided scale is pale in the middle, where white borders would vanish
      // thin dark borders keep neighbours with similar colours apart and pale areas off the page
      Plot.geo(fc.features, { fill: (f) => row(f)?.[A.fill.field], stroke: INK, strokeWidth: 0.4 }),
      // highlight: colour is the data here, so highlighted regions get a dark outline
      // (white under dark, so that the outline shows on both ends of the colour scale)
      hi ? Plot.geo(fc.features.filter((f) => hi(row(f))), { fill: "none", stroke: BG, strokeWidth: 5 }) : null,
      hi ? Plot.geo(fc.features.filter((f) => hi(row(f))), { fill: "none", stroke: INK, strokeWidth: 2.5 }) : null,
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

// Population pyramid: x = age (vertical axis, one bar per age), y = count, fill = the two sides (first value on
// the left), frame = the year shown (`data` holds that year only). Rows whose linetype is not its first value
// (e.g. a projection) are drawn pale. options.reference: a frame drawn as an outline over the others.
function pyramidChart(spec, data, A, o, width, all) {
  const age = A.x.field, y = A.y.field, side = label(A.fill), frame = A.frame.field;
  const sides = [...new Set(all.map((d) => d[side]))];
  const sign = (d) => (d[side] === sides[0] ? -1 : 1);
  const max = d3.max(all, (d) => d[y]);
  const kind = A.linetype && label(A.linetype);
  const firstKind = kind ? all[0][kind] : null;
  const pale = kind && data[0]?.[kind] !== firstKind;
  const now = data[0]?.[frame];
  // outline of the reference frame: a step line along the ages of each side
  const ref = o.reference != null && String(o.reference) !== String(now)
    ? d3.sort(all.filter((d) => String(d[frame]) === String(o.reference)), (d) => d[age])
      .flatMap((d) => [0, 1].map((k) => ({ s: d[side], x: sign(d) * d[y], y: d[age] + k })))
    : [];
  const ages = d3.extent(all, (d) => d[age]);
  const corner = [fmt(A.frame, now), pale ? data[0][kind] : null].filter(Boolean).join(" · ");

  return Plot.plot({
    ...base(o, width),
    height: o.height ?? Math.min(560, Math.max(380, width * 0.75)),
    marginLeft: 40,
    x: {
      domain: [-max, max], nice: true, grid: true, label: shown(A.y) ? withUnit(A.y) : null, labelAnchor: "center",
      ticks: Math.max(4, Math.floor(width / 90)),
      tickFormat: (v) => fmt(A.y, Math.abs(v), { tick: true }),
    },
    y: { domain: [ages[0], ages[1] + 1], label: shown(A.x) ? A.x.label : null, ticks: d3.range(0, ages[1] + 1, 10), tickFormat: (v) => fmt(A.x, v) },
    color: colourScale(all, A.fill, o),
    marks: [
      Plot.rectX(data, {
        x1: 0, x2: (d) => sign(d) * d[y], y1: (d) => d[age], y2: (d) => d[age] + 1, inset: 0,
        fill: side, fillOpacity: pale ? 0.4 : 0.9,
        title: tipText(spec.aes), tip: { pointer: "y" },
      }),
      Plot.ruleX([0], { stroke: BG, strokeWidth: 1 }),
      ref.length ? Plot.line(ref, { x: "x", y: "y", z: "s", stroke: INK, strokeWidth: 1.2 }) : null,
      Plot.text([corner], { frameAnchor: "top-right", fontSize: 18, fontWeight: 600, fill: INK, dy: 4 }),
      ref.length ? Plot.text([`Obrys: ${fmt(A.frame, o.reference)}`], { frameAnchor: "top-right", dy: 30, fill: INK_2, fontSize: 12 }) : null,
    ],
  });
}

// options.sort_last: categories that stay at the end of a sorted bar chart (e.g. "Ostatní")
function sortLast(cats, o) {
  const last = [o.sort_last ?? []].flat().map(String);
  return [...cats.filter((c) => !last.includes(String(c))), ...cats.filter((c) => last.includes(String(c)))];
}

const TYPES = { line: lineChart, bar: barChart, dot: dotChart, map: mapChart, pyramid: pyramidChart };

// ---------- facets (small multiples) ----------
// options.facet = TRUE adds a toggle that splits a line or dot chart (by colour) or a stacked bar chart (by fill) into one
// small chart per series, three across (two on narrow screens or for four series). Each panel has its own y scale
// (it is there to show each series' trend); options.facet_scale = "shared" puts every panel on the same scale,
// so sizes can be compared. Each panel's title gives the series and its latest value.
const facetAes = (spec, A) => (spec.type === "bar" ? A.fill ?? A.colour : spec.type === "line" || spec.type === "dot" ? A.colour : null);
// A one-hue scheme ("reds", "blues"…) starts almost white, which vanishes into the page: on maps it starts at a
// visible tint instead (the palest 20 % of the scheme is skipped)
const ONE_HUE = ["blues", "greens", "greys", "oranges", "purples", "reds"];
function paleStart(scheme, diverging) {
  if (diverging || typeof scheme !== "string" || !ONE_HUE.includes(scheme.toLowerCase())) return null;
  const f = d3[`interpolate${scheme[0].toUpperCase()}${scheme.slice(1).toLowerCase()}`];
  return (t) => f(0.2 + 0.8 * t);
}

// `text` shortened with "…" to at most `max` px (in the current font of `measure`)
function clip(text, max) {
  if (measure.measureText(text).width <= max) return text;
  while (text.length > 1 && measure.measureText(text + "…").width > max) text = text.slice(0, -1);
  return text.trimEnd() + "…";
}

function facetChart(spec, data, A, o, width, all) {
  const by = facetAes(spec, A), key = label(by), x = A.x.field, y = A.y.field;
  const series = [...new Set(all.map((d) => d[key]))]; // order and colours of the whole dataset
  const n = series.length;
  const cols = Math.min(n, o.facet_columns ?? 3, width >= 720 && n !== 4 ? 3 : 2), gap = 24;
  const pw = Math.floor((width - gap * (cols - 1)) / cols);
  const extent = (rows) => [Math.min(0, d3.min(rows, (d) => d[y]) ?? 0), Math.max(0, d3.max(rows, (d) => d[y]) ?? 1)];
  const shared = o.facet_scale === "shared" ? extent(data) : null;
  const xs = d3.sort(new Set(data.map((d) => d[x])));
  const title = tipText(spec.aes);
  // highlight: bold category label, as in a bar chart whose fill is taken
  const hi = highlighter(A);
  // categories instead of years (e.g. countries): each panel is a horizontal bar chart, sorted highest first
  const textual = typeof xs[0] === "string";
  // a dot chart (one row per category) always splits into horizontal bar panels
  const horizontal = spec.type === "dot" || (spec.type === "bar" && (o.horizontal ?? (textual && xs.length > 6)));
  if (horizontal) {
    measure.font = `13px ${FONT}`;
    // row labels get up to half a panel; longer ones are shortened with "…"
    const labelW = Math.min(Math.max(160, Math.floor(pw / 2)), Math.ceil(d3.max(xs, (c) => measure.measureText(String(c)).width)) + 12);
    const rowLabel = (c) => { measure.font = `13px ${FONT}`; return clip(String(c), labelW - 12); };
    const panels = series.map((s, i) => {
      const rows = data.filter((d) => d[key] === s);
      const colour = seriesColour(by, s, i);
      measure.font = `600 13.5px ${FONT}`;
      const head = clip(String(s), pw - 4);
      const dom = shared ?? extent(rows);
      const order = sortLast(d3.sort(rows, (d) => -d[y]).map((d) => d[x]), o);
      const panel = Plot.plot({
        ...base(o, pw),
        height: order.length * 18 + 62,
        marginTop: 32,
        marginLeft: labelW,
        marginRight: 30, // room for half of the last tick label
        // ticks with the unit ("200 mld. Kč"): only as many as fit side by side
        x: { label: null, grid: true, tickFormat: tickOf(A.y), domain: dom, nice: true,
          ticks: fitTicks(A.y, dom, { ...o, y_domain: dom }, pw - labelW) },
        y: { label: null, domain: order, tickSize: 0, tickFormat: rowLabel },
        marks: [
          Plot.barX(rows, { y: x, x: y, fill: colour, title, tip: true }),
          Plot.ruleX([0]),
          Plot.text([head], { frameAnchor: "top-left", dx: -labelW, dy: -26, fontWeight: 600, fontSize: 13.5, fill: "currentColor", text: (d) => d }),
        ],
      });
      if (hi) boldTicks(panel, rows.filter(hi).map((d) => d[x]));
      return panel;
    });
    return h("div", { class: "chart-facets", style: `grid-template-columns: repeat(${cols}, ${pw}px); column-gap: ${gap}px` }, panels);
  }
  const panels = series.map((s, i) => {
    const rows = data.filter((d) => d[key] === s);
    const last = rows.length ? rows.reduce((a, b) => (b[x] > a[x] ? b : a)) : null;
    measure.font = `600 13.5px ${FONT}`;
    const head = clip(String(s), pw - 4); // the series name only (its latest value is in the tooltip)
    const colour = seriesColour(by, s, i);
    const dom = shared ?? extent(rows);
    // ticks with the unit; the left margin below fits the widest one
    const yTick = tickOf(A.y);
    const yScale = { label: null, grid: true, tickFormat: yTick, domain: dom, nice: true, ticks: 3 };
    measure.font = `13px ${FONT}`; // the chart font size; + tick (6 px), its padding and a little room
    const left = Math.ceil(d3.max(d3.scaleLinear(dom).nice(3).ticks(3), (t) => measure.measureText(yTick(t)).width)) + 24;
    const every = Math.ceil(xs.length / Math.max(2, Math.floor((pw - left) / 42))); // year labels that fit
    const marks = spec.type === "bar"
      ? [Plot.barY(rows, { x, y, fill: colour, title, tip: true }), Plot.ruleY([0])]
      : [Plot.ruleY([0]), Plot.line(rows, { x, y, stroke: colour, strokeWidth: 2 }),
        Plot.dot(last ? [last] : [], { x, y, fill: colour, r: 3.5 }),
        Plot.tip(rows, Plot.pointerX({ x, y, title }))];
    return Plot.plot({
      ...base(o, pw),
      height: 176,
      marginTop: 32,
      marginLeft: left,
      x: spec.type === "bar"
        ? { label: null, domain: xs, ticks: xs.filter((_, j) => j % every === 0), tickFormat: (d) => fmt(A.x, d, { tick: true }) }
        : { label: null, tickFormat: tickOf(A.x), ticks: Math.max(2, Math.floor(pw / 80)) },
      y: yScale,
      marks: [
        ...marks,
        // panel title inside the SVG, so it is also in the PNG
        Plot.text([head], { frameAnchor: "top-left", dx: -left, dy: -26, fontWeight: 600, fontSize: 13.5, fill: "currentColor", text: (d) => d }),
      ],
    });
  });
  return h("div", { class: "chart-facets", style: `grid-template-columns: repeat(${cols}, ${pw}px); column-gap: ${gap}px` }, panels);
}

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

// Table columns: the aesthetics (keys first: x, geometry, frame; then categories; then values) and the extra
// columns of options.table (each {field, label, unit, format}). Highlight flags, geometry codes that have a
// labeller and columns nobody described are left out.
function tableColumns(spec, data) {
  const isText = (f) => typeof data.find((d) => d[f] != null)?.[f] === "string";
  const col = (a) => ({ field: a.labeller ?? a.field, a: a.labeller ? null : a, label: a.label });
  const aes = spec.aes.filter((a) => a.type !== "highlight" && data.columns.includes(a.labeller ?? a.field));
  const keys = aes.filter((a) => ["x", "geometry", "frame"].includes(a.type));
  const rest = aes.filter((a) => !keys.includes(a));
  const cols = [...keys, ...rest.filter((a) => isText(a.field)), ...rest.filter((a) => !isText(a.field))].map(col);
  for (const a of spec.options?.table ?? []) if (data.columns.includes(a.field)) cols.push({ field: a.field, a, label: a.label });
  return cols.filter((c, i) => cols.findIndex((d) => d.field === c.field) === i);
}

function dataTable(spec, data) {
  const cols = tableColumns(spec, data);
  const head = (c) => (c.a ? withUnit(c.a) : c.label);
  const cell = (c, v) => (c.a ? fmt(c.a, v) : raw(v));
  return h("table", { class: "chart-table" },
    h("thead", {}, h("tr", {}, cols.map((c) => h("th", {}, head(c))))),
    h("tbody", {}, data.map((d) =>
      h("tr", {}, cols.map((c) => h("td", { class: typeof d[c.field] === "number" ? "num" : null }, cell(c, d[c.field])))))));
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

// Slider with one handle over `values`, starting at `start`; calls onChange(value)
function frameSlider(values, start, onChange) {
  const last = values.length - 1;
  const input = h("input", { type: "range", min: 0, max: last, step: 1, value: Math.max(0, values.indexOf(start)), "aria-label": "Rok" });
  const text = h("span", { class: "timeline-year" });
  const track = h("div", { class: "timeline-track" }, h("div", { class: "timeline-fill" }), input);
  const sync = (moved) => {
    const i = +input.value;
    text.textContent = values[i];
    track.style.setProperty("--to", last ? i / last : 1);
    if (moved) onChange(values[i]);
  };
  input.addEventListener("input", () => sync(true));
  sync(false);
  return h("div", { class: "chart-timeline" }, track, text);
}

// PNG = title, subtitle, legends, chart and source, with the site's mark in the top right corner (the version for
// a light background). The chart is drawn again off screen at a fixed width, so every export has the same
// proportions whatever the size of the screen it came from
const LOGO = new URL("img/cesko-v-cislech-znacka-svetla.svg", import.meta.url).href;
const EXPORT_WIDTH = 760;

// Splits `text` into lines no wider than `max` (in the current font of `ctx`)
function wrapLines(ctx, text, max) {
  const lines = [];
  let line = "";
  for (const word of String(text ?? "").split(/\s+/).filter(Boolean)) {
    const next = line ? `${line} ${word}` : word;
    if (line && ctx.measureText(next).width > max) { lines.push(line); line = word; } else line = next;
  }
  return line ? [...lines, line] : lines;
}

// An SVG drawn as an image cannot reach the page's web fonts: the Inter of the page (assets/fonts/inter.css) goes in
// as data (once)
let fontCss;
const embeddedFont = () => (fontCss ??= (async () => {
  const link = document.querySelector('link[rel="stylesheet"][href*="fonts/inter.css"]');
  if (!link) return "";
  const css = await (await fetch(link.href)).text();
  const rules = css.split("}").filter((r) => /@font-face/.test(r));
  const out = await Promise.all(rules.map(async (rule) => {
    const url = rule.match(/url\(["']?([^"')]+)["']?\)/)?.[1];
    if (!url) return "";
    const blob = await (await fetch(new URL(url, link.href))).blob();
    const data = await new Promise((resolve) => { const r = new FileReader(); r.onload = () => resolve(r.result); r.readAsDataURL(blob); });
    return rule.replace(url, data) + "}";
  }));
  return out.join("\n");
})().catch(() => "")); // the system font rather than no PNG

const svgImage = async (svg, w, h) => {
  const copy = svg.cloneNode(true);
  const font = await embeddedFont();
  if (font) copy.prepend(Object.assign(document.createElementNS("http://www.w3.org/2000/svg", "style"), { textContent: font }));
  copy.setAttribute("xmlns", "http://www.w3.org/2000/svg");
  copy.setAttribute("width", w);
  copy.setAttribute("height", h);
  copy.style.color = INK;
  const src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(new XMLSerializer().serializeToString(copy));
  // the embedded font loads only once the image exists: the first copy can still show the fallback font,
  // so it is drawn once and thrown away, and the second copy is used
  let img;
  for (let i = 0; i < (font ? 2 : 1); i++) {
    img = new Image();
    img.src = src;
    await img.decode();
    if (font && i === 0) {
      document.createElement("canvas").getContext("2d").drawImage(img, 0, 0);
      await new Promise((r) => setTimeout(r, 60));
    }
  }
  return img;
};

// `render(width)` returns the nodes of the chart (legends and plot) drawn at that width
async function downloadPng(spec, render, host) {
  const stage = h("div", { class: "chart-body", "aria-hidden": "true",
    style: `position: absolute; left: -100000px; top: 0; width: ${EXPORT_WIDTH}px` });
  document.body.append(stage); // not inside the card, whose zoom would scale the export
  try {
    stage.replaceChildren(...(await render(EXPORT_WIDTH)));
    await document.fonts?.ready;
    const frame = stage.getBoundingClientRect();
    const at = (r) => ({ x: r.left - frame.left, y: r.top - frame.top, w: r.width, h: r.height });
    // every svg (plot, panels, legend swatches and colour ramps) where the browser laid it out
    const svgs = await Promise.all([...stage.querySelectorAll("svg")]
      .filter((svg) => !svg.parentElement.closest("svg") && svg.getBoundingClientRect().width > 0)
      .map(async (svg) => { const r = at(svg.getBoundingClientRect()); return { ...r, img: await svgImage(svg, r.w, r.h) }; }));
    // the text of HTML legends (swatch labels, facet titles), each line where the browser put it
    const lines = [];
    const fit = document.createElement("canvas").getContext("2d");
    const walker = document.createTreeWalker(stage, NodeFilter.SHOW_TEXT);
    for (let node; (node = walker.nextNode());) {
      if (!node.data.trim() || node.parentElement.closest("svg")) continue;
      const style = getComputedStyle(node.parentElement);
      if (style.visibility === "hidden") continue;
      const range = document.createRange();
      range.selectNodeContents(node);
      const rects = [...range.getClientRects()].filter((r) => r.width > 0);
      const font = `${style.fontWeight} ${style.fontSize} ${FONT}`;
      const words = node.data.trim().split(/\s+/);
      fit.font = font;
      rects.forEach((r, i) => { // a text that wraps has one rect per line: give each line the words that fit it
        let line = words.shift() ?? "";
        while (words.length && (i === rects.length - 1 || fit.measureText(`${line} ${words[0]}`).width <= r.width + 2)) line += ` ${words.shift()}`;
        lines.push({ ...at(r), text: line, font, color: style.color });
      });
    }

    const logo = new Image();
    logo.src = LOGO;
    const hasLogo = await logo.decode().then(() => true, () => false); // no logo rather than no PNG
    const width = EXPORT_WIDTH, height = frame.height, pad = 28, scale = 2;
    const logoH = 40, logoW = hasLogo ? (logoH * logo.naturalWidth) / logo.naturalHeight : 0;
    const measure = document.createElement("canvas").getContext("2d");
    measure.font = `600 22px ${FONT}`;
    const titleLines = wrapLines(measure, plain(spec.title), width - (hasLogo ? logoW + 20 : 0));
    measure.font = `15px ${FONT}`;
    const subLines = wrapLines(measure, plain(spec.subtitle), width - (hasLogo ? logoW + 20 : 0));
    measure.font = `13px ${FONT}`;
    const names = sourceGroups(spec).map(([name]) => name).join(", ");
    // the page the chart is on, e.g. ceskovcislech.org/hdp_rust (set in _layouts/default.html)
    const address = document.documentElement.dataset.address || "ceskovcislech.org";
    const sourceLines = wrapLines(measure, `Zdroj: ${names} · ${address.replace(/\/$/, "")}`, width);
    const top = pad + titleLines.length * 28 + subLines.length * 21 + 14;
    const bottom = 16 + sourceLines.length * 18 + pad - 6;

    const canvas = h("canvas", { width: (width + 2 * pad) * scale, height: Math.ceil(top + height + bottom) * scale });
    const ctx = canvas.getContext("2d");
    ctx.scale(scale, scale);
    ctx.fillStyle = BG;
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    ctx.textBaseline = "alphabetic";
    let y = pad + 20;
    ctx.fillStyle = INK;
    ctx.font = `600 22px ${FONT}`;
    for (const line of titleLines) { ctx.fillText(line, pad, y); y += 28; }
    ctx.fillStyle = INK_2;
    ctx.font = `15px ${FONT}`;
    y -= 4;
    for (const line of subLines) { ctx.fillText(line, pad, y); y += 21; }
    // the top of the logo on the top of the title's letters
    ctx.font = `600 22px ${FONT}`;
    const titleTop = pad + 20 - (ctx.measureText(titleLines[0] ?? "").actualBoundingBoxAscent || 15);
    if (hasLogo) ctx.drawImage(logo, pad + width - logoW, titleTop, logoW, logoH);
    for (const p of svgs) ctx.drawImage(p.img, pad + p.x, top + p.y, p.w, p.h);
    ctx.textBaseline = "middle";
    for (const t of lines) {
      ctx.font = t.font;
      ctx.fillStyle = t.color;
      ctx.fillText(t.text, pad + t.x, top + t.y + t.h / 2);
    }
    ctx.textBaseline = "alphabetic";
    ctx.fillStyle = INK_3;
    ctx.font = `13px ${FONT}`;
    y = top + height + 24;
    for (const line of sourceLines) { ctx.fillText(line, pad, y); y += 18; }
    h("a", { href: canvas.toDataURL("image/png"), download: `${spec.id}.png` }).click();
  } finally {
    stage.remove();
  }
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
    // Tabs: a switch in the chart; each tab draws the aesthetics with its `tab` plus those without one
    const tabList = spec.tabs ?? [];
    let tab = tabList[0]?.id;
    let aes, A; // aesthetics of the current tab, and the same by type
    const setTab = (id) => {
      tab = id;
      aes = spec.aes.filter((a) => a.tab == null || !tabList.length || a.tab === tab);
      A = Object.fromEntries(aes.map((a) => [a.type, a]));
    };
    setTab(tab);
    if (o.filter_latest) { // keep only the newest period of the given column
      const latest = d3.max(data, (d) => d[o.filter_latest]);
      data = Object.assign(data.filter((d) => +d[o.filter_latest] === +latest), { columns: data.columns });
    }
    for (const t of [spec.type, ...tabList.map((t) => t.type).filter(Boolean)]) {
      if (!TYPES[t]) throw new Error(`neznámý typ grafu „${t}“`);
    }

    const body = h("div", { class: "chart-body" });
    const subset = (keep) => Object.assign(data.filter(keep), { columns: data.columns });

    // Frame slider (pyramid): one value of the frame column at a time. Starts at options.frame, else at the
    // last frame of the first linetype value (the last actual year before a projection), else at the last one.
    const ff = A.frame?.field;
    const frames = ff ? d3.sort(new Set(data.map((d) => d[ff]))) : [];
    const kind = A.linetype && label(A.linetype);
    const start = o.frame ?? (kind ? d3.max(data.filter((d) => d[kind] === data[0][kind]), (d) => d[ff]) : frames.at(-1));
    let shown = ff ? subset((d) => d[ff] === start) : data, stale = false;
    const table = h("div", { class: "chart-table-wrap", hidden: true }, dataTable(spec, shown));

    // Year slider: any line or bar chart whose x is a year (options.timeline = false turns it off)
    const xf = A.x?.field; // tabs share the x column
    const years = (spec.type === "line" || spec.type === "bar") && A.x?.format === "year" && o.timeline !== false
      ? d3.sort(new Set(data.map((d) => d[xf]).filter(Number.isFinite))) : [];
    const update = (keep) => {
      shown = subset(keep);
      stale = true;
      table.replaceChildren(dataTable(spec, shown));
      draw();
    };
    const slider = ff
      ? frames.length > 1 && frameSlider(frames, start, (v) => update((d) => d[ff] === v))
      : years.length > 2 && yearSlider(years, spec.type === "line" ? 1 : 0, (a, b) => update((d) => d[xf] >= a && d[xf] <= b));
    // Facet toggle (options.facet): one small chart per series, and back
    let faceted = false;
    const facetButton = o.facet && facetAes(spec, A) && h("button", {
      class: "chart-tab chart-facet-toggle", type: "button", "aria-pressed": "false", title: "Samostatný graf pro každou řadu",
      onclick: () => {
        faceted = !faceted;
        facetButton.setAttribute("aria-pressed", String(faceted));
        stale = true;
        draw();
      },
    });
    if (facetButton) facetButton.innerHTML = '<svg width="13" height="13" viewBox="0 0 14 14" aria-hidden="true"><rect x="1" y="1" width="5" height="5" rx="1"/><rect x="8" y="1" width="5" height="5" rx="1"/><rect x="1" y="8" width="5" height="5" rx="1"/><rect x="8" y="8" width="5" height="5" rx="1"/></svg><span>Rozdělit</span>';
    const facetWrap = facetButton && h("div", { class: "chart-tabs" }, facetButton);
    const tabs = ["Graf", "Tabulka"].map((name, i) =>
      h("button", {
        class: "chart-tab", type: "button", "aria-pressed": String(i === 0),
        onclick: () => {
          tabs.forEach((t, j) => t.setAttribute("aria-pressed", String(i === j)));
          body.hidden = i !== 0;
          if (switcher) switcher.hidden = i !== 0;
          if (facetWrap) facetWrap.hidden = i !== 0;
          table.hidden = i !== 1;
        },
      }, name));
    const switcher = tabList.length > 1 && h("div", { class: tabList.length > 3 ? "chart-switch chart-switch-many" : "chart-switch", role: "group", "aria-label": "Zobrazený ukazatel" },
      tabList.map((t) => h("button", {
        class: "chart-tab", type: "button", "aria-pressed": String(t.id === tab),
        onclick: (e) => {
          for (const b of switcher.children) b.setAttribute("aria-pressed", String(b === e.currentTarget));
          setTab(t.id);
          stale = true;
          draw();
        },
      }, plain(t.label))));
    // the PNG shows the tab on screen, named after the subtitle
    // a full stop at the end of the subtitle would sit before the tab name ("… práce. – Mzda")
    const pngSpec = () => ({ ...spec, subtitle: [plain(spec.subtitle).replace(/\.\s*$/, ""), tabList.find((t) => t.id === tab)?.label].filter(Boolean).join(" – ") });

    el.classList.add("chart-card");
    el.replaceChildren(...[
      h("div", { class: "chart-head" },
        h("div", {},
          h("h3", { class: "chart-title" }, rich(spec.title)),
          spec.subtitle ? h("p", { class: "chart-subtitle" }, rich(spec.subtitle)) : null),
        h("div", { class: "chart-controls" }, facetWrap || null, h("div", { class: "chart-tabs" }, tabs))),
      switcher,
      body,
      table,
      slider,
      h("div", { class: "chart-footer" },
        sourceLine(spec),
        h("div", { class: "chart-actions" },
          h("a", { class: "chart-action", href: csvUrl, download: `${id}.csv` }, "Stáhnout data (CSV)"),
          h("button", { class: "chart-action", type: "button", onclick: () => downloadPng(pngSpec(), nodesAt, el) }, "Stáhnout PNG"))),
    ].filter(Boolean));

    let drawn = 0;
    async function draw() { // on first show, on resize and when the year range changes
      const width = Math.round(body.clientWidth);
      if (!width || (width === drawn && !stale)) return; // zero width = the table tab is open
      drawn = width;
      stale = false;
      body.replaceChildren(...(await nodesAt(width)));
    }
    async function nodesAt(width) { // the chart (with its legends) as drawn at `width`
      // tabs of a line or bar chart: only the rows that have this tab's y (a tab may use its own rows)
      const has = tabList.length && A.y ? (d) => d[A.y.field] != null : null;
      // a tab may draw as another chart type (tabs = list(list(id, label, type = "bar")))
      const type = tabList.find((t) => t.id === tab)?.type ?? spec.type;
      const tspec = { ...spec, type, aes };
      const chart = faceted && facetAes(tspec, A) ? facetChart : TYPES[type];
      // annotations with a `tab` are drawn in that tab only
      const ot = o.annotations ? { ...o, annotations: o.annotations.filter((a) => a.tab == null || a.tab === tab) } : o;
      const plot = await chart(tspec, has ? shown.filter(has) : shown, A, ot, width, has ? data.filter(has) : data);
      // gridlines: Plot draws them at 10 % opacity, barely visible on the tinted background; set on the elements
      // (not in CSS) so the PNG export has them too
      for (const g of plot.querySelectorAll?.('[aria-label$="-grid"]') ?? []) g.setAttribute("stroke-opacity", 0.22);
      return [type === "line" && !faceted && dashLegend(data, A), plot].filter(Boolean);
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
