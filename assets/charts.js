// Charts of this site: the Charta renderer (lib/charta, a git submodule; docs in lib/charta/README.md)
// with the site's branding. Data in chart_data/, map boundaries in geo/, both in the site root.
import { configure, renderAll } from "../lib/charta/js/charts.js";

configure({
  root: new URL("..", import.meta.url), // site root, wherever the page lives
  logo: new URL("img/cesko-v-cislech-znacka-svetla.svg", import.meta.url),
  // the page the chart is on, e.g. ceskovcislech.org/hdp_rust (data-address is set in _layouts/default.html)
  address: document.documentElement.dataset.address || "ceskovcislech.org",
});
renderAll();
