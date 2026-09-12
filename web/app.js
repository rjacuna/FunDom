// FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
// Copyright (C) 2026 RJ Acuña. SPDX-License-Identifier: GPL-3.0-or-later
// The single-page version. The same Haskell that serves the native page is a
// wasm32-wasi reactor module with exports — render, svg, identifyKey — and
// this file is the whole difference: it owns the URL, hands the module the
// query string and (in mode 2) the table data, and puts the HTML on the page.
// Every link and form the Haskell renders is relative and stateless, so this
// is just interception plus history.pushState.
import { wasiImports } from "./wasi-shim.js";

let hs = null;
let csg = null;

async function load() {
  if (hs) return hs;
  if (!loading) loading = instantiate();
  return loading;
}
let loading = null;
async function instantiate() {
  // compiled as it downloads when the server says application/wasm, else after
  const mod = WebAssembly.compileStreaming
    ? await WebAssembly.compileStreaming(fetch("./fundom.wasm")).catch(async () => WebAssembly.compile(await (await fetch("./fundom.wasm")).arrayBuffer()))
    : await WebAssembly.compile(await (await fetch("./fundom.wasm")).arrayBuffer());
  const { wasi, setMemory } = wasiImports(mod);
  const { default: ghcJsffi } = await import("./fundom.js");
  const jsffi = {};
  const inst = await WebAssembly.instantiate(mod, {
    wasi_snapshot_preview1: wasi,
    ghc_wasm_jsffi: ghcJsffi(jsffi),
  });
  Object.assign(jsffi, inst.exports);
  setMemory(inst.exports.memory);
  inst.exports._initialize();          // reactor ABI: exactly once
  hs = inst.exports;
  return hs;
}

// The Cummins–Pauli tables, exported by `fundom csg-export`; fetched the
// first time mode 2 is used.
async function tables() {
  if (csg) return csg;
  csg = await (await fetch("./csg.json")).json();
  return csg;
}

// The compact formats Modular.CP parses.
const count1 = xs => xs.filter(x => x === 1).length;
const compactRecord  = r => [r.n, r.l, r.i, r.g, r.m, r.mg.map(m => m.join(",")).join(";"),
                             r.cu.join(" "), r.c2.join(" "), r.c3.join(" "), r.s,
                             r.con, r.len, r.gal.join(" "), r.sup.join(" "), r.sub.join(" ")].join("|");
const compactSummary = r => [r.n, r.l, r.i, r.g, r.cu.join(" "), r.s, r.con, r.len, r.gal.join(" "),
                             count1(r.c2), count1(r.c3), r.sup.join(" "), r.sub.join(" ")].join("|");

// What the module needs beside the query: in mode 2, one record (the
// chosen group) and, as one string, the option lists for the dropdowns plus
// the summaries of every group matching the chosen filters — the same
// computation as Modular.CP.filterAndOptions, over the JSON.
async function inputs(query) {
  const p = new URLSearchParams(query);
  const app = p.get("app") || (p.has("gens") ? "3" : (p.has("db") || p.has("gen") || p.has("lev") || p.has("idx") ? "2" : "1"));
  if (app !== "2") {
    // modes 1 and 3: the table entries with this group's genus, level,
    // index and cusp widths, for the module to compare against
    const key = (await load()).identifyKey(query.replace(/^\?/, ""));
    if (!key) return { record: "", summaries: "" };
    const [g, l, i, ws] = key.split("|");
    const widths = ws.split(" ").map(Number).sort((a, b) => a - b).join(" ");
    const data = await tables();
    const cands = data.filter(x => x.g === +g && x.l === +l && x.i === +i && [...x.cu].sort((a, b) => a - b).join(" ") === widths);
    return { record: "", summaries: [...cands.map(r => "!" + compactRecord(r)), ...classical(data, cands)].join("\n") };
  }
  const data = await tables();
  const name = (p.get("db") || "").toUpperCase();
  const num = k => (p.get(k) || "") === "" ? null : +p.get(k);
  let f = { g: num("gen"), l: num("lev"), i: num("idx") };
  let record = "", names = [];
  if (name) {
    const r = data.find(x => x.n === name);
    if (r) {
      record = compactRecord(r);
      names = classical(data, [r]);
      if (f.g === null && f.l === null && f.i === null) f = { g: r.g, l: r.l, i: r.i };
    }
  }
  const fits = (x, ff) => (ff.g === null || x.g === ff.g) && (ff.l === null || x.l === ff.l) && (ff.i === null || x.i === ff.i);
  const distinct = (key, ff) => [...new Set(data.filter(x => fits(x, ff)).map(x => x[key]))].sort((a, b) => a - b);
  const header = "#gen " + distinct("g", { ...f, g: null }).join(" ")
               + "|lev " + distinct("l", { ...f, l: null }).join(" ")
               + "|idx " + distinct("i", { ...f, i: null }).join(" ");
  const matching = data.filter(x => fits(x, f)).map(compactSummary);              // every group when nothing is chosen
  return { record, summaries: [header, ...matching, ...names].join("\n") };
}

// The classical names among the super- and subgroups of some records, as
// the ~name|special lines Modular.CP.parseNames reads.
function classical(data, recs) {
  const ns = new Set(recs.flatMap(r => [...r.sup, ...r.sub]));
  return data.filter(x => x.s && ns.has(x.n)).map(x => "~" + x.n + "|" + x.s);
}

function spinner() {
  const plot = document.getElementById("plot");
  if (!plot) return;
  const svg = plot.querySelector("svg");
  const w = svg ? svg.getAttribute("width") : 900, h = svg ? svg.getAttribute("height") : 520;
  plot.innerHTML = '<div class="plot-wait" style="width:' + w + 'px;height:' + h + 'px"><div class="spin"></div></div>';
}

// A short yield so the spinner paints before the synchronous render. Not
// requestAnimationFrame alone: that never fires while the tab is hidden.
const nextFrame = () => new Promise(r => {
  let done = false;
  const go = () => { if (!done) { done = true; r(); } };
  requestAnimationFrame(() => setTimeout(go, 0));
  setTimeout(go, 60);
});

function show(html) {
  const doc = new DOMParser().parseFromString(html, "text/html");
  document.title = doc.title || "Fundamental domains";
  let style = document.getElementById("fundom-style");
  if (!style) { style = document.createElement("style"); style.id = "fundom-style"; document.head.appendChild(style); }
  style.textContent = Array.from(doc.querySelectorAll("style")).map(s => s.textContent).join("\n");
  document.body.innerHTML = doc.body.innerHTML;
  if (window.fundomMath) window.fundomMath(document.body);   // KaTeX, once it has loaded
  window.scrollTo(0, 0);
}

// Pages rendered at build time (window.fundomPre, put in the shell by
// wasm/shell.py): the default page, the empty tables and generators pages,
// and the worked examples.  They are shown at once, module or no module.
const pre = new Map(), preHtml = new Map();
function preKey(query) {                                        // an empty value is the default, as for the module
  const p = new URLSearchParams(query.replace(/^\?/, ""));
  return new URLSearchParams([...p.entries()].filter(([, v]) => v !== "").sort()).toString();
}
for (const [q, file] of (window.fundomPre || [])) pre.set(preKey(q), file);

async function render(query) {
  const file = pre.get(preKey(query));
  if (file !== undefined) {
    if (!preHtml.has(file)) preHtml.set(file, await (await fetch("./" + file)).text());
    show(preHtml.get(file));
    return;
  }
  spinner();
  await nextFrame();                   // let the spinner paint before the (synchronous) render
  const m = await load();
  const { record, summaries } = await inputs(query);
  show(m.render(query.replace(/^\?/, ""), record, summaries));
}

function go(query) {
  history.pushState(null, "", query || "?");
  render(query).catch(fail);
}

async function openSvg(query) {
  const m = await load();
  const { record, summaries } = await inputs(query);
  const svg = m.svg(query.replace(/^\?/, ""), record, summaries);
  window.open(URL.createObjectURL(new Blob([svg], { type: "image/svg+xml" })), "_blank");
}

function fail(err) {
  const plot = document.getElementById("plot") || document.body;
  plot.innerHTML = '<div class="panel warn" style="margin:12px">' + String(err) + "</div>";
  console.error(err);
}

document.addEventListener("click", ev => {
  const a = ev.target.closest("a[href]");
  if (!a) return;
  const href = a.getAttribute("href");
  if (href.startsWith("?")) { ev.preventDefault(); go(href); }
  else if (href.startsWith("svg?")) { ev.preventDefault(); openSvg(href.slice(3)).catch(fail); }
});

document.addEventListener("submit", ev => {
  const f = ev.target;
  if (!(f instanceof HTMLFormElement) || f.method.toLowerCase() !== "get") return;
  ev.preventDefault();
  go("?" + new URLSearchParams(new FormData(f)).toString());
});

window.addEventListener("popstate", () => render(location.search).catch(fail));

// The shell is the default page itself; anything else is rendered now.  The
// module and the tables load in the background meanwhile, for the first
// page that needs them.
if (preKey(location.search) !== preKey("")) render(location.search).catch(fail);
load().then(tables).catch(() => {});
