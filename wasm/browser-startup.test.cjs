// Browser regression checks; run with Playwright installed and the web bundle served.
const assert = require('node:assert/strict');
const {chromium} = require('playwright');
(async () => {
 const b = await chromium.launch(process.env.CHROME_PATH ? {executablePath: process.env.CHROME_PATH} : {});
 try {
  const fc=await b.newContext();await fc.addInitScript(() => {
    window.__warmups = [];
    window.requestIdleCallback = callback => { window.__warmups.push(callback); return window.__warmups.length; };
    window.__fetches = [];
    const original = window.fetch;
    window.fetch = (input, options) => {
      window.__fetches.push({url: String(input), priority: options?.priority});
      return original(input, options);
    };
  });const f=await fc.newPage();const fr=[],fe=[];f.on('request',r=>fr.push(r.url()));f.on('pageerror',e=>fe.push(String(e)));
  await f.route('**/*.wasm*',r=>r.abort());await f.route('**/csg.json*',r=>r.abort());
  await f.goto((process.env.FUNDOM_URL || 'http://localhost:8092/') + '');await f.waitForFunction(()=>!document.querySelector('[data-part]'));
  await f.goto((process.env.FUNDOM_URL || (process.env.FUNDOM_URL || 'http://localhost:8092/') + '') + '?n=5');await f.waitForFunction(()=>document.querySelector('#plot svg')&&!document.querySelector('[data-part]'));
  assert.equal(fr.filter(u=>u.includes('.wasm')||u.includes('csg.json')).length,0);
  console.log('FunDom: default and n=5 complete without WASM/tables PASS');
  await f.unroute('**/*.wasm*');await f.unroute('**/csg.json*');
  await f.goto((process.env.FUNDOM_URL || (process.env.FUNDOM_URL || 'http://localhost:8092/') + '') + '?n=24&view=uhp');
  await f.waitForFunction(()=>document.querySelector('#plot svg')&&document.body.innerText.includes('Γ₀(24) index 48')&&!document.querySelector('.plot-wait'),{},{timeout:30000});
  await f.waitForFunction(async()=>{const c=await caches.open('fundom-assets-v1');return (await c.keys()).some(r=>r.url.includes('fundom.wasm'))&&(await c.keys()).some(r=>r.url.includes('csg.json'))});
  await f.route('**/*.wasm*',r=>r.abort());await f.route('**/csg.json*',r=>r.abort());fr.length=0;
  await f.reload();await f.waitForFunction(()=>document.querySelector('#plot svg')&&document.body.innerText.includes('Γ₀(24) index 48')&&!document.querySelector('.plot-wait'),{},{timeout:30000});
  assert.equal(fr.filter(u=>u.includes('.wasm')||u.includes('csg.json')).length,0);assert.deepEqual(fe,[]);
  console.log('FunDom: automatic low-priority warmup, arbitrary n=24, persistent WASM/table cache PASS');await fc.close();
 } finally { await b.close(); }
})().catch(e => { console.error(e); process.exitCode = 1; });
