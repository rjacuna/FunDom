// FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
// Copyright (C) 2026 RJ Acuña. SPDX-License-Identifier: GPL-3.0-or-later
// The single-page version. The same Haskell that serves the native page is a
// wasm32-wasi reactor module with exports — render, svg, identifyKey — and
// this file is the whole difference: it owns the URL, hands the module the
// query string and (in mode 2) the table data, and puts the HTML on the page.
// Every link and form the Haskell renders is relative and stateless, so this
// is just interception plus history.pushState.
import { wasiImports } from "./wasi-shim.js";
import { asset, afterInitialView } from "./assets.js?v=background-1";

let hs = null;
let csg = null;

async function load(priority = "auto") {
  if (hs) return hs;
  if (!loading) loading = instantiate(priority).catch(e => { loading = null; throw e; });
  return loading;
}
let loading = null;
async function instantiate(priority) {
  // compiled as it downloads when the server says application/wasm, else after
  const response = await asset("fundom.wasm", undefined, priority);
  if (!response.ok) throw new Error(`calculation engine download: HTTP ${response.status}`);
  const mod = WebAssembly.compileStreaming && response.headers.get("Content-Type")?.split(";")[0] === "application/wasm"
    ? await WebAssembly.compileStreaming(response)
    : await WebAssembly.compile(await response.arrayBuffer());
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
async function tables(priority = "auto") {
  if (csg) return csg;
  csg = asset("csg.json", undefined, priority).then(r => {
    if (!r.ok) throw new Error(`tables download: HTTP ${r.status}`);
    return r.json();
  }).catch(e => { csg = null; throw e; });
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
  // the genus, level and index filter the list of groups; the group chosen from it does not set them
  const f = { g: num("gen"), l: num("lev"), i: num("idx") };
  let record = "", names = [];
  if (name) {
    const r = data.find(x => x.n === name);
    if (r) {
      record = compactRecord(r);
      names = classical(data, [r]);
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
  const styles = Array.from(doc.querySelectorAll("style")).map(s => s.textContent).join("\n");
  if (styles) style.textContent = styles;
  document.body.innerHTML = doc.body.innerHTML;
  if (window.fundomMath) window.fundomMath(document.body);   // KaTeX, once it has loaded
  window.scrollTo(0, 0);
}

// Shipped plots are final AVIF pictures, with compact gzip page data. SVG
// is computed only when an interaction needs it (explorer, editing, custom views).
const pre = new Map(), preHtml = new Map();
const preDefaults = { g1:"G0", g2:"G0", n:"1", m:"1", app:"1", view:"disk", tile:"1", bg:"1",
  fill:"banana", outline:"black", fill2:"chocolate", c1:"white", c2:"grey", w:"900", h:"520",
  mode:"domain", edit:"0", links:"0", cusps:"1" };
function preKey(query) {
  const p = new URLSearchParams(query.replace(/^\?/, ""));
  const app=p.get("app") || (p.get("gens") ? "3" : ["db","gen","lev","idx"].some(k=>p.get(k)) ? "2" : "1");
  p.set("app",app);
  if(app!=="1") for(const k of ["g1","g2","n","m"]) p.delete(k);
  if(app!=="3") p.delete("gens");
  if(app!=="2") for(const k of ["db","gen","lev","idx"]) p.delete(k);
  if (p.get("db")) p.set("db",p.get("db").toUpperCase());
  const disk = p.get("view") !== "uhp";
  return new URLSearchParams([...p.entries()].filter(([k,v])=>v!=="" && v!==preDefaults[k] &&
    !(disk && (k==="scale" || k==="cx"))).sort()).toString();
}
for (const [q,file] of (window.fundomPre || [])) pre.set(preKey(q),file);
async function compressed(file) {
  const r = await fetch("./"+file);
  if (!r.ok) throw Object.assign(new Error(`${file}: HTTP ${r.status}`),{status:r.status});
  return new Response(r.body.pipeThrough(new DecompressionStream("gzip"))).text();
}
function pageText(file) {
  if (!preHtml.has(file)) preHtml.set(file,compressed(file).catch(e=>{preHtml.delete(file);throw e;}));
  return preHtml.get(file);
}
let cataloguePromise;
function catalogue() {
  if (!cataloguePromise) cataloguePromise=compressed("static/catalog.json.gz").then(JSON.parse)
    .catch(e=>{cataloguePromise=null;throw e;});
  return cataloguePromise;
}
function displayName(r) {
  let text=r.s||r.n;
  for(const [from,to] of [["\\overline",""],["\\Gamma","Γ"],["\\cap","∩"],["\\pm","±"],
    ["_0","₀"],["_1","₁"],["^0","⁰"],["^1","¹"],["^2","²"],["^3","³"],["\\,"," "],["\\ "," "]]) text=text.replaceAll(from,to);
  return text;
}
function tableMenu(data, query) {
  for(const select of document.querySelectorAll('.pills select')) select.disabled=false;
  const p=new URLSearchParams(query), names={gen:"g",lev:"l",idx:"i"};
  const fits=(r,except)=>Object.entries(names).every(([k,v])=>k===except || !p.get(k) || String(r[v])===p.get(k));
  const matching=data.filter(r=>fits(r));
  for(const [key,field] of Object.entries(names)) {
    const select=document.querySelector(`select[name="${key}"]`);
    if (!select) continue;
    const values=[...new Set(data.filter(r=>fits(r,key)).map(r=>r[field]))].sort((a,b)=>a-b);
    select.replaceChildren(new Option("any",""),...values.map(v=>new Option(v,String(v))));
    select.value=p.get(key)||"";
  }
  const select=document.querySelector('select[name="db"]');
  if (select) {
    const selected=(p.get("db")||"").toUpperCase(), records=[...matching];
    const outside=data.find(r=>r.n===selected && !fits(r));
    if (outside) records.unshift(outside);
    records.sort((a,b)=>a.g-b.g||a.l-b.l||a.i-b.i||a.n.localeCompare(b.n));
    select.replaceChildren(new Option(matching.length?"group…":"no match",""),
      ...records.map(r=>new Option(`${displayName(r)} · index ${r.i}, genus ${r.g}, level ${r.l}${r===outside?" · outside these filters":""}`,r.n)));
    select.value=selected;
    const count=select.closest("form").nextElementSibling;
    count.textContent=Object.keys(names).some(k=>p.get(k))
      ? `${matching.length} group${matching.length===1?" matches":"s match"}.`
      : `${matching.length} groups: every congruence subgroup of PSL₂(ℤ) of genus ≤ 24, up to conjugacy. Each choice narrows the others.`;
  }
  // Filters describe the menu, not the group or its picture. Carry them
  // through view changes, including forms generated without these filters.
  for (const a of document.querySelectorAll('a[href^="?"]')) {
    const target=new URLSearchParams(a.getAttribute("href").slice(1));
    if (target.get("app")!=="2") continue;
    for(const k of Object.keys(names)) {target.delete(k);if(p.get(k)) target.set(k,p.get(k));}
    a.setAttribute("href","?"+target);
  }
  for(const form of document.querySelectorAll('form')) {
    if(form.querySelector('input[name="app"]')?.value!=="2") continue;
    if(form.querySelector('input[name="db"]')) continue; // the by-name form starts without category filters
    for(const k of Object.keys(names)) {
      if (form.querySelector(`[name="${k}"]`)) continue;
      if(p.get(k)){const input=document.createElement("input");input.type="hidden";input.name=k;input.value=p.get(k);form.appendChild(input);}
    }
  }
}
// Upper-half-plane links are pinned to their fitted scale/centre by the
// native renderer. Recognize those exact values; zoomed/panned views use SVG.
function fitted(html, p) {
  if (!p.has("scale") && !p.has("cx")) return true;
  const m=html.match(/href="svg\?([^"]*)"/);
  if (!m) return false;
  const fit=new URLSearchParams(m[1].replaceAll("&amp;","&"));
  return ["scale","cx"].every(k=>!p.has(k) || p.get(k)===(fit.get(k)||(k==="cx"?"0":"50")));
}
async function shipped(query) {
  const key=preKey(query), p=new URLSearchParams(key), table=p.get("app")==="2";
  const lookup=new URLSearchParams(p);
  if(table) for(const k of ["gen","lev","idx"]) lookup.delete(k);
  if(p.get("view")==="uhp") {lookup.delete("tile");lookup.delete("bg");}
  let file=pre.get(lookup.toString()), tableFile=false;
  if (!file && p.get("view")==="uhp") {
    lookup.delete("scale");lookup.delete("cx");
    file=pre.get(preKey(lookup.toString()));
  }
  if (!file && table && p.get("db")) {
    const allowed=new Set(["app","db","view","tile","bg","scale","cx","gen","lev","idx"]);
    if ([...p.keys()].some(k=>!allowed.has(k))) return null;
    if (p.has("view") && p.get("view")!=="uhp") return null;
    if (["tile","bg"].some(k=>p.has(k) && p.get(k)!=="0")) return null;
    if (!/^[0-9]+[A-Z]+[0-9]+$/.test(p.get("db"))) return null;
    const v=p.get("view")==="uhp"?4:(p.get("tile")==="0"?1:0)+(p.get("bg")==="0"?2:0);
    file=`static/tables/${p.get("db")}-${v}.html.gz`;
    tableFile=true;
  }
  if (!file) return null;
  let html;
  try {html=await pageText(file);}
  catch(e) {if(tableFile && e.status===404) return null;throw e;}
  if(p.get("view")==="uhp" && !fitted(html,p)) return null;
  return {html,table};
}
let revision=0;
async function completePicture() {
  const img=document.querySelector("#plot img");
  if(img) {
    const background=img.dataset.background;
    await Promise.all([img.decode(),background ? new Promise((resolve,reject)=>{
      const image=new Image();image.onload=resolve;image.onerror=reject;image.src="./"+background;
    }) : Promise.resolve()]);
  }
}
async function render(query) {
  const current=++revision;
  const ready=await shipped(query);
  if(current!==revision) return;
  if(ready) {
    show(ready.html);
    for(const p of document.querySelectorAll('p.muted'))
      if(p.textContent==="Click a triangle to see its matrix and its side pairings.")
        p.textContent="Choose Explore triangles to see each matrix and its side pairings.";
    if(ready.table) {
      for(const select of document.querySelectorAll('.pills select')) select.disabled=true;
      const count=document.querySelector('.pills')?.closest('form').nextElementSibling;
      if(count) count.textContent="Loading table filters…";
    }
    // Disk background preferences have no effect on a half-plane picture,
    // but should survive switching back to the disk.
    const state=new URLSearchParams(query);
    const inert=state.get("view")==="uhp"?["tile","bg"]:[];
    if(["2","3"].includes(new URLSearchParams(preKey(query)).get("app"))) inert.push("g1","g2","n","m");
    for(const key of inert) {
      if(!state.has(key)) continue;
      for(const a of document.querySelectorAll('a[href^="?"]')) {
        const target=new URLSearchParams(a.getAttribute("href").slice(1));
        target.set(key,state.get(key));a.setAttribute("href","?"+target);
      }
      for(const form of document.querySelectorAll('form')) {
        let input=form.querySelector(`[name="${key}"]`);
        if(!input) {input=document.createElement("input");input.type="hidden";input.name=key;form.appendChild(input);}
        input.value=state.get(key);
      }
    }
    await completePicture();
    if(ready.table) {
      // A shipped picture needs neither the full mathematical tables nor
      // the compact filter catalogue. Populate the menus after it appears.
      catalogue().then(data=>{if(current===revision) tableMenu(data,query);}).catch(e=>{
        if(current===revision) {
          const count=document.querySelector('.pills')?.closest('form').nextElementSibling;
          if(count) count.textContent="Table filters could not be loaded. Change view to retry.";
        }
        console.error(e);
      });
    }
    return;
  }
  spinner();await nextFrame();
  const [m]=await Promise.all([load(),tables()]);
  const {record,summaries}=await inputs(query);
  if(current===revision) show(m.render(query.replace(/^\?/,""),record,summaries));
}

function go(query) {
  const from=new URLSearchParams(location.search), to=new URLSearchParams(query);
  if(to.get("view")==="uhp" && (from.get("view")!=="uhp" ||
    ["n","m","g1","g2","db","gens"].some(k=>from.get(k)!==to.get(k)))) {
    to.delete("scale");to.delete("cx");query="?"+to;
  }
  history.pushState(null, "", query || "?");
  render(query).catch(fail);
}

async function openSvg(query) {
  const [m] = await Promise.all([load(), tables()]);
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

// Finish the initial view first. Warm the engine and tables concurrently at
// low network priority while the visitor explores; actions reuse these promises.
const initialView = location.search !== ""
  ? render(location.search) : completePicture();
initialView.then(() => afterInitialView(() => Promise.all([load("low"), tables("low"), catalogue()]))).catch(fail);
