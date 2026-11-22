// Home page: the ORP hex map that turns from the flag into data and back (d3 transitions).
// Geometry: assets/orp-hex.json (206 hexes, kraj borders, outline); data: the ORP map charts in chart_data/.
import * as d3 from "https://cdn.jsdelivr.net/npm/d3@7.9.0/+esm";

const figure = document.querySelector("#orp-map");
const ROOT = new URL("..", import.meta.url);
const INK = "#161616", RULE = "#C9C5BC";
// kraj borders and the outline: a dark grey line on a light halo, so it reads on the flag and on the light and dark
// ends of every colour scale
const LINE = "#45433E", HALO = "#FBFAF7";
// "Přechod 1 · Prolínání" from the design: the colours cross-fade, the caption slides out and in at the start
const INTERVAL = 10000;
const MAP = 2200, KRAJE = 1400, LEGEND = 1000, LEGEND_FADE = 800; // ms: half the speed of the design
const CAP_OUT = 480, CAP_IN = 600, CAP_SHIFT = 6;                // caption: out upwards, the new one in from below
// book: the link under the map leads to an article
const BOOK = '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="square" aria-hidden="true"><path d="M2 4h7a3 3 0 0 1 3 3v13a2 2 0 0 0-2-2H2z"/><path d="M22 4h-7a3 3 0 0 0-3 3v13a2 2 0 0 1 2-2h8z"/></svg>';

const number = (v, digits) => v.toLocaleString("cs-CZ", { minimumFractionDigits: digits, maximumFractionDigits: digits });
const signed = (v) => (v > 0 ? "+" : v < 0 ? "−" : "") + number(Math.abs(v * 100), 1) + " %";

// One state per view of the map. `colour` turns a value into a fill; `legend` = the 5 swatches beside the map, highest
// first, with `labels` at the top, middle and bottom.
const STATES = [
  { title: "206 správních obvodů ORP", sub: "", link: "Témata", href: "#temata" },
  {
    title: "Změna počtu obyvatel", sub: "2015–2025, %", article: "obyvatele",
    csv: "obyvatele-zmena-orp.csv", field: "zmena", format: signed,
    colour: d3.scaleLinear([-0.15, -0.075, 0, 0.075, 0.15], ["#A82424", "#E4665E", "#F2EFE6", "#7D9CC5", "#11457E"])
      .interpolate(d3.interpolateLab).clamp(true),
    legend: ["#5278A8", "#ABBCD3", "#F4D5CA", "#E5675F", "#AC2726"], labels: ["+15 %", "0", "−15 %"],
  },
  {
    title: "Naděje dožití při narození", sub: "2021–2025, roky", article: "zdravi",
    csv: "zdravi-nadeje-orp.csv", field: "nadeje", format: (v) => number(v, 1) + " let",
    colour: d3.scaleThreshold([77, 78, 79, 80], ["#E4ECF6", "#B9CEE7", "#8BACD5", "#5D88C0", "#2F62A3"]),
    legend: ["#2F62A3", "#5D88C0", "#8BACD5", "#B9CEE7", "#E4ECF6"], labels: ["80+", "78–79", "< 77"],
  },
  {
    title: "Index sociálního vyloučení", sub: "2024, vyšší = horší", article: "chudoba",
    csv: "chudoba-isv-orp.csv", field: "isv", format: (v) => number(v, 1),
    colour: d3.scaleLinear([0, 10, 20], ["#F7E0DA", "#DB4A43", "#870B11"]).interpolate(d3.interpolateLab).clamp(true),
    legend: ["#9F2020", "#C13934", "#E05F56", "#F0A79F", "#F6DBD5"], labels: ["20+", "10", "0"],
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

  const svg = d3.select(figure).select(".orp-svg").attr("viewBox", geo.viewBox);
  const hexes = svg.append("g").selectAll("path").data(geo.hexes).join("path")
    .attr("d", (h) => h.d).attr("stroke-width", 0.6)
    .attr("fill", fill(STATES[0])).attr("stroke", stroke(STATES[0]))
    .on("pointerenter", (_, h) => { hover = h; caption(); });
  const kraje = svg.append("g").attr("pointer-events", "none").attr("opacity", 0); // kraj borders, over the data only
  for (const [stroke, width] of [[HALO, 1.6], [LINE, 0.7]]) {
    kraje.append("g").selectAll("line").data(geo.kraje).join("line")
      .attr("x1", (l) => l[0]).attr("y1", (l) => l[1]).attr("x2", (l) => l[2]).attr("y2", (l) => l[3])
      .attr("stroke", stroke).attr("stroke-width", width).attr("stroke-linecap", "round");
  }
  for (const [stroke, width] of [[HALO, 2.2], [LINE, 1.1]]) {
    svg.append("path").attr("d", geo.outline).attr("fill", "none").attr("stroke", stroke)
      .attr("stroke-width", width).attr("stroke-linejoin", "round").attr("pointer-events", "none");
  }

  const $ = (sel) => figure.querySelector(sel);
  // legend: 5 fixed swatches whose colours blend into the next view's; it fades out on the flag
  const legend = d3.select($(".orp-legend")).style("opacity", 0);
  const swatches = legend.select("svg").selectAll("rect").data(d3.range(5)).join("rect")
    .attr("y", (k) => k * 24).attr("width", 8).attr("height", 23).attr("fill", "transparent");
  const buttons = d3.select($(".orp-states")).selectAll("button").data(STATES).join("button")
    .attr("type", "button").attr("aria-label", (s) => s.title)
    .on("click", (_, s) => { show(STATES.indexOf(s)); restart(); });
  buttons.append("span");

  const reduce = matchMedia("(prefers-reduced-motion: reduce)").matches;
  let i = 0, hover = null, paused = false, timer = null;
  let cur = STATES[0]; // the state the caption describes: changes once the old caption has slid out
  const link = $(".orp-link");
  link.innerHTML = `${BOOK}<span></span>`;
  const cap = d3.select($(".orp-cap"));

  function caption() {
    const s = cur;
    const v = hover && s.values?.get(hover.kod);
    $(".orp-title").textContent = hover ? hover.name : s.title;
    $(".orp-sub").textContent = hover ? (v != null ? `${s.title}: ${s.format(v)}` : s.title) : s.sub;
  }
  function show(next, animate = !reduce) {
    i = next;
    const s = STATES[i];
    const ms = (v) => (animate ? v : 0);
    hexes.attr("stroke", stroke(s)) // outlines of the white stripe: at once
      .transition("map").duration(ms(MAP)).ease(d3.easeCubicOut).attr("fill", fill(s));
    kraje.transition("map").duration(ms(KRAJE)).attr("opacity", s.values ? 1 : 0);
    svg.attr("aria-label", `Mapa ORP: ${s.title}`);
    buttons.attr("aria-pressed", (_, k) => String(k === i)).select("span").style("background", (_, k) => (k === i ? INK : RULE));
    // legend: the colours blend; on the flag it fades out and keeps the last labels meanwhile
    legend.transition("fade").duration(ms(LEGEND_FADE)).style("opacity", s.legend ? 1 : 0);
    if (s.legend) {
      swatches.transition("fill").duration(ms(LEGEND)).attr("fill", (k) => s.legend[k]);
      ["hi", "mid", "lo"].forEach((c, k) => { $(`.orp-${c}`).textContent = s.labels[k]; });
    }
    // the link changes at once
    link.href = s.href;
    link.querySelector("svg").hidden = !s.values; // the book only for an article
    link.querySelector("span").textContent = `${s.link} →`;
    // caption: slides up and out, the new one comes in from below
    if (!animate) { cur = s; caption(); return; }
    cap.interrupt("cap").transition("cap").duration(CAP_OUT).ease(d3.easeCubicIn)
      .style("opacity", 0).style("transform", `translateY(${-CAP_SHIFT}px)`)
      .end().then(() => {
        cur = s;
        caption();
        cap.style("transform", `translateY(${CAP_SHIFT}px)`)
          .transition("cap").duration(CAP_IN).ease(d3.easeCubicOut)
          .style("opacity", 1).style("transform", "translateY(0px)");
      }, () => {}); // interrupted by the next change: that one writes the caption
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
