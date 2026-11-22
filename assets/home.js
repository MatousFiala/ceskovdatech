// Home page: the ORP hex map that turns from the flag into data and back (d3 transitions).
// Geometry: assets/orp-hex.json (206 hexes, kraj borders, outline); data: the ORP map charts in chart_data/.
import * as d3 from "https://cdn.jsdelivr.net/npm/d3@7.9.0/+esm";

const figure = document.querySelector("#orp-map");
const ROOT = new URL("..", import.meta.url);
const INK = "#161616", RULE = "#C9C5BC";
const DURATION = 1600, INTERVAL = 9000;

const number = (v, digits) => v.toLocaleString("cs-CZ", { minimumFractionDigits: digits, maximumFractionDigits: digits });
const signed = (v) => (v > 0 ? "+" : v < 0 ? "−" : "") + number(Math.abs(v * 100), 1) + " %";

// One state per view of the map. `colour` turns a value into a fill; `legend` = the 10 swatches under the map.
const STATES = [
  { title: "206 správních obvodů ORP", sub: "Lorem ipsum dolor sit amet", link: "Témata", href: "#temata" },
  {
    title: "Změna počtu obyvatel", sub: "2015–2025, %", article: "obyvatele",
    csv: "obyvatele-zmena-orp.csv", field: "zmena", format: signed,
    colour: d3.scaleLinear([-0.15, -0.075, 0, 0.075, 0.15], ["#A82424", "#E4665E", "#F2EFE6", "#7D9CC5", "#11457E"])
      .interpolate(d3.interpolateLab).clamp(true),
    legend: ["#AC2726", "#C94943", "#E5675F", "#F0A094", "#F4D5CA", "#DBDEE0", "#ABBCD3", "#7C9BC5", "#5278A8", "#29568C"],
    lo: "−15 %", mid: "0", hi: "+15 %",
  },
  {
    title: "Naděje dožití při narození", sub: "2021–2025, roky", article: "zdravi",
    csv: "zdravi-nadeje-orp.csv", field: "nadeje", format: (v) => number(v, 1) + " let",
    colour: d3.scaleThreshold([77, 78, 79, 80], ["#D9E4F1", "#9FBADB", "#5F8DC2", "#2A5E9A", "#0B2F57"]),
    legend: ["#D9E4F1", "#D9E4F1", "#9FBADB", "#9FBADB", "#5F8DC2", "#5F8DC2", "#2A5E9A", "#2A5E9A", "#0B2F57", "#0B2F57"],
    lo: "< 77", mid: "78–79", hi: "80+",
  },
  {
    title: "Index sociálního vyloučení", sub: "2024, vyšší = horší", article: "chudoba",
    csv: "chudoba-isv-orp.csv", field: "isv", format: (v) => number(v, 1),
    colour: d3.scaleLinear([0, 10, 20], ["#F7E0DA", "#DB4A43", "#870B11"]).interpolate(d3.interpolateLab).clamp(true),
    legend: ["#F6DBD5", "#F4C1BA", "#F0A79F", "#E9847B", "#E05F56", "#D2443E", "#C13934", "#B02D2A", "#9F2020", "#8E1315"],
    lo: "0", mid: "10", hi: "20+",
  },
];

if (figure) start().catch((err) => console.error("Mapa ORP:", err));

async function start() {
  const geo = await d3.json(new URL("assets/orp-hex.json", ROOT));
  const tables = await Promise.all(STATES.map((s) => s.csv ? d3.csv(new URL(`chart_data/${s.csv}`, ROOT), d3.autoType) : null));
  STATES.forEach((s, i) => {
    if (!tables[i]) return;
    s.values = new Map(tables[i].map((d) => [String(d.orp_kod), d[s.field]]));
    s.href = figure.dataset[`href${s.article[0].toUpperCase()}${s.article.slice(1)}`] ?? "#";
    s.link = figure.dataset[`link${s.article[0].toUpperCase()}${s.article.slice(1)}`] ?? s.title;
  });
  const fill = (s) => (h) => (s.values ? (s.values.has(h.kod) ? s.colour(s.values.get(h.kod)) : "#EEEDE8") : h.flag);
  const stroke = (s) => (h) => (s.values ? "none" : h.flagStroke);

  const svg = d3.select(figure).select("svg").attr("viewBox", geo.viewBox);
  const hexes = svg.append("g").selectAll("path").data(geo.hexes).join("path")
    .attr("d", (h) => h.d).attr("stroke-width", 0.6)
    .attr("fill", fill(STATES[0])).attr("stroke", stroke(STATES[0]))
    .on("pointerenter", (_, h) => { hover = h; caption(); });
  const kraje = svg.append("g").attr("pointer-events", "none").attr("opacity", 0); // kraj borders, over the data only
  kraje.selectAll("line").data(geo.kraje).join("line")
    .attr("x1", (l) => l[0]).attr("y1", (l) => l[1]).attr("x2", (l) => l[2]).attr("y2", (l) => l[3])
    .attr("stroke", INK).attr("stroke-width", 0.9).attr("stroke-linecap", "round");
  svg.append("path").attr("d", geo.outline).attr("fill", "none").attr("stroke", INK)
    .attr("stroke-width", 1.3).attr("stroke-linejoin", "round").attr("pointer-events", "none");

  const $ = (sel) => figure.querySelector(sel);
  const swatches = d3.select($(".orp-legend svg")).selectAll("rect").data(d3.range(10)).join("rect")
    .attr("x", (i) => i * 16).attr("width", 16).attr("height", 8);
  const buttons = d3.select($(".orp-states")).selectAll("button").data(STATES).join("button")
    .attr("type", "button").attr("aria-label", (s) => s.title)
    .on("click", (_, s) => { show(STATES.indexOf(s)); restart(); });
  buttons.append("span");

  const reduce = matchMedia("(prefers-reduced-motion: reduce)").matches;
  let i = 0, hover = null, paused = false, timer = null;

  function caption() {
    const s = STATES[i];
    const v = hover && s.values?.get(hover.kod);
    $(".orp-title").textContent = hover ? hover.name : s.title;
    $(".orp-sub").textContent = hover ? (v != null ? `${s.title}: ${s.format(v)}` : s.title) : s.sub;
  }
  function show(next, animate = !reduce) {
    i = next;
    const s = STATES[i];
    const t = svg.transition().duration(animate ? DURATION : 0).ease(d3.easeCubicInOut);
    hexes.transition(t).attr("fill", fill(s)).attr("stroke", stroke(s));
    kraje.transition(t).attr("opacity", s.values ? 1 : 0);
    svg.attr("aria-label", `Mapa ORP: ${s.title}`);
    caption();
    $(".orp-legend").hidden = !s.legend;
    if (s.legend) {
      swatches.attr("fill", (k) => s.legend[k]);
      $(".orp-lo").textContent = s.lo; $(".orp-mid").textContent = s.mid; $(".orp-hi").textContent = s.hi;
    }
    buttons.attr("aria-pressed", (_, k) => String(k === i)).select("span").style("background", (_, k) => (k === i ? INK : RULE));
    const a = $(".orp-link");
    a.href = s.href;
    a.textContent = `${s.link} →`;
  }
  function restart() {
    clearInterval(timer);
    if (!reduce) timer = setInterval(() => { if (!paused) show((i + 1) % STATES.length); }, INTERVAL);
  }
  figure.addEventListener("pointerenter", () => { paused = true; });
  figure.addEventListener("pointerleave", () => { paused = false; hover = null; caption(); });

  show(0, false);
  restart();
}
