// The single-page version. The same Haskell that serves the native page is a
// wasm32-wasi reactor module with three exports — render, svg, level — and
// this file is the whole difference: it owns the URL, hands the module the
// query string and (in mode 2) the table data, and puts the HTML on the page.
// Every link and form the Haskell renders is relative and stateless, so this
// is just interception plus history.pushState.
import { wasiImports } from "./wasi-shim.js";

let hs = null;
let csg = null;

async function load() {
  if (hs) return hs;
  const bytes = await (await fetch("./fundom.wasm")).arrayBuffer();
  const mod = await WebAssembly.compile(bytes);
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
const compactRecord  = r => [r.n, r.l, r.i, r.g, r.m, r.mg.map(m => m.join(",")).join(";"),
                             r.cu.join(" "), r.c2.join(" "), r.c3.join(" "), r.s].join("|");
const compactSummary = r => [r.n, r.l, r.i, r.g, r.cu.join(" "), r.s].join("|");

// What the module needs beside the query: one record and the summaries of
// one genus, or nothing.
async function inputs(query) {
  const p = new URLSearchParams(query);
  const app = p.get("app") || (p.has("gens") ? "3" : (p.has("db") || p.has("gen") ? "2" : "1"));
  if (app !== "2") return { record: "", summaries: "" };
  const data = await tables();
  const name = (p.get("db") || "").toUpperCase();
  let record = "", genus = p.get("gen");
  if (name) {
    const r = data.find(x => x.n === name);
    if (r) { record = compactRecord(r); if (genus === null) genus = String(r.g); }
  }
  const summaries = genus === null ? "" : data.filter(x => x.g === +genus).map(compactSummary).join("\n");
  return { record, summaries };
}

function show(html) {
  const doc = new DOMParser().parseFromString(html, "text/html");
  document.title = doc.title || "Fundamental domains";
  let style = document.getElementById("fundom-style");
  if (!style) { style = document.createElement("style"); style.id = "fundom-style"; document.head.appendChild(style); }
  style.textContent = Array.from(doc.querySelectorAll("style")).map(s => s.textContent).join("\n");
  document.body.innerHTML = doc.body.innerHTML;
  window.scrollTo(0, 0);
}

async function render(query) {
  const p = new URLSearchParams(query);
  const m = await load();
  if (p.has("list")) {
    // the listing of one level, a JS-only route
    const lv = p.get("list");
    const data = await tables();
    show(m.level(String(+lv), data.filter(x => x.l === +lv).map(compactSummary).join("\n")));
    return;
  }
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
  document.body.innerHTML = '<div style="padding:20px;color:#b00;font-family:sans-serif">' + String(err) + "</div>";
  console.error(err);
}

document.addEventListener("click", ev => {
  const a = ev.target.closest("a[href]");
  if (!a) return;
  const href = a.getAttribute("href");
  if (href.startsWith("?")) { ev.preventDefault(); go(href); }
  else if (href.startsWith("svg?")) { ev.preventDefault(); openSvg(href.slice(3)).catch(fail); }
  else if (href.startsWith("csg?level=")) { ev.preventDefault(); go("?list=" + href.slice("csg?level=".length)); }
});

document.addEventListener("submit", ev => {
  const f = ev.target;
  if (!(f instanceof HTMLFormElement) || f.method.toLowerCase() !== "get") return;
  ev.preventDefault();
  go("?" + new URLSearchParams(new FormData(f)).toString());
});

window.addEventListener("popstate", () => render(location.search).catch(fail));

render(location.search).catch(fail);
