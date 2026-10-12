// Run against the served web bundle, with Playwright installed.
const assert=require('node:assert/strict');
const {chromium}=require('playwright');
const base=process.env.FUNDOM_URL || 'http://localhost:8092/';
(async()=>{
 const browser=await chromium.launch(process.env.CHROME_PATH?{executablePath:process.env.CHROME_PATH}:{});
 try {
  const context=await browser.newContext();
  await context.addInitScript(()=>{
   window.__warmups=[];
   window.requestIdleCallback=callback=>{window.__warmups.push(callback);return window.__warmups.length;};
   window.__fetches=[];
   const fetch=window.fetch;
   window.fetch=(input,options)=>{window.__fetches.push({url:String(input),priority:options?.priority});return fetch(input,options);};
  });
  const page=await context.newPage(), requests=[], errors=[];
  page.on('request',r=>requests.push(r.url()));page.on('pageerror',e=>errors.push(String(e)));
  await page.route('**/*.wasm*',r=>r.abort());await page.route('**/csg.json*',r=>r.abort());
  const picture=async q=>{
   await page.goto(base+q,{waitUntil:'domcontentloaded'});
   await page.waitForFunction(()=>{const i=document.querySelector('#plot img');return i?.complete&&i.naturalWidth>0&&!document.querySelector('#plot svg');});
  };
  // Even the small browsing catalogue must not gate a selected picture.
  let releaseCatalogue;
  const pausedCatalogue=new Promise(r=>{releaseCatalogue=r;});
  await page.route('**/static/catalog.json.gz',async r=>{await pausedCatalogue;await r.continue();});
  await picture('?app=2&db=11A1');
  assert.equal(await page.locator('.pills select:disabled').count(),4);
  releaseCatalogue();await page.waitForFunction(()=>document.querySelector('select[name="db"]')?.options.length>8000);
  await page.unroute('**/static/catalog.json.gz');
  assert.equal(await page.locator('.pills select:disabled').count(),0);
  await page.goto(base+'?app=2',{waitUntil:'domcontentloaded'});
  await page.waitForFunction(()=>document.querySelector('select[name="db"]')?.options.length>8000);
  assert.equal(await page.locator('#plot img,#plot svg').count(),0);
  console.log('Empty table browser uses the compact catalogue: PASS');
  requests.length=0;
  console.log('Selected table picture appears before the filter catalogue: PASS');
  for(const q of ['?n=3&scale=400', '', '?n=1', '?n=23&m=22', '?n=5&tile=0', '?n=5&bg=0', '?n=5&tile=0&bg=0', '?n=5&view=uhp', '?n=5&view=uhp&tile=0&bg=0',
    '?app=2&db=11A1','?app=2&db=11A1&g1=G1&n=5','?db=11A1&gen=1&lev=11&idx=12','?app=2&db=11A1&view=uhp']) {
    await picture(q);
    if(q==='?n=3&scale=400') {
      const html=require('node:fs').readFileSync(require('node:path').join(__dirname,'../web/index.html'),'utf8');
      const defaultPicture=html.match(/data-src="\.\/([^"]+)"/)[1];
      assert.ok(!requests.some(u=>u.endsWith(defaultPicture)),'A deep link downloaded the unused default picture');
    }
  }
  await page.waitForFunction(()=>document.querySelector('select[name="db"]').options.length>100);
  const manifest=JSON.parse(require('node:zlib').gunzipSync(require('node:fs').readFileSync(require('node:path').join(__dirname,'../web/static/manifest.json.gz'))));
  for(const [q] of manifest.filter(([q])=>{const p=new URLSearchParams(q);return p.get('app')==='3'&&p.get('gens');})) await picture('?'+q);
  // Return to a table entry for the filter/history checks below.
  await picture('?app=2&db=11A1&view=uhp');
  await page.waitForFunction(()=>document.querySelector('select[name="db"]').options.length>100);
  assert.equal(requests.filter(u=>/\.wasm|csg\.json|\.svg/.test(u)).length,0);
  console.log('Static families, disk variants, UHP and table records without WASM or SVG: PASS');
  await page.selectOption('select[name="gen"]','0');
  await page.waitForFunction(()=>document.querySelector('select[name="gen"]')?.value==='0'&&document.body.innerText.includes('outside these filters'));
  assert.equal(await page.locator('#plot img').count(),1);
  await page.goBack();await page.waitForFunction(()=>document.querySelector('select[name="gen"]')?.value==='');
  console.log('Table filters preserve the selected group; history restores menus: PASS');
  // A fitted UHP URL stays static, but changing the scale requests a live SVG.
  await page.unroute('**/*.wasm*');await page.unroute('**/csg.json*');
  await page.waitForFunction(()=>window.__warmups.length>0);
  await page.evaluate(()=>window.__warmups.splice(0).forEach(f=>f()));
  await page.waitForFunction(async()=>{const c=await caches.open('fundom-assets-v1'),keys=await c.keys();return keys.some(r=>r.url.includes('fundom.wasm'))&&keys.some(r=>r.url.includes('csg.json'));});
  const priorities=await page.evaluate(()=>window.__fetches.filter(r=>/\.wasm|csg\.json/.test(r.url)).map(r=>r.priority));
  assert.ok(priorities.length>=2&&priorities.every(p=>p==='low'));
  await page.getByRole('link',{name:'Explore triangles',exact:true}).click();
  await page.waitForFunction(()=>document.querySelector('#plot svg')&&!document.querySelector('.plot-wait'));
  assert.ok(await page.locator('#plot svg a').count()>0);
  console.log('Automatic background warmup and interactive domain SVG: PASS');
  await page.goto(base+'?n=3',{waitUntil:'domcontentloaded'});
  await page.getByRole('link',{name:'triangle explorer',exact:true}).click();
  await page.waitForFunction(()=>document.querySelector('#plot svg')&&!document.querySelector('.plot-wait'));
  console.log('Triangle explorer: PASS');
  await page.goto(base+'?n=24&view=uhp',{waitUntil:'domcontentloaded'});
  await page.waitForFunction(()=>document.querySelector('#plot svg')&&document.body.innerText.includes('Γ₀(24) index 48')&&!document.querySelector('.plot-wait'));
  await page.route('**/*.wasm*',r=>r.abort());await page.route('**/csg.json*',r=>r.abort());requests.length=0;
  await page.reload({waitUntil:'domcontentloaded'});
  await page.waitForFunction(()=>document.querySelector('#plot svg')&&document.body.innerText.includes('Γ₀(24) index 48')&&!document.querySelector('.plot-wait'));
  assert.equal(requests.filter(u=>/\.wasm|csg\.json/.test(u)).length,0);assert.deepEqual(errors,[]);
  console.log('Unshipped views and persistent engine/table cache: PASS');
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
