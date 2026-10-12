// Verify complete shipped coverage and that plots contain only raster images.
const fs=require('node:fs/promises'),path=require('node:path'),assert=require('node:assert/strict');
const {gunzipSync}=require('node:zlib'),sharp=require('sharp');
const web=path.resolve(__dirname,'../web'),dir=path.join(web,'static');
(async()=>{
 const read=async p=>gunzipSync(await fs.readFile(path.join(web,p))).toString();
 const full=JSON.parse(await fs.readFile(path.join(web,'csg.json'),'utf8'));
 const catalog=JSON.parse(await read('static/catalog.json.gz'));
 assert.deepEqual(catalog.map(r=>r.n),full.map(r=>r.n));
 const manifest=JSON.parse(await read('static/manifest.json.gz'));
 const family=new Set(manifest.map(([q])=>{
  const p=new URLSearchParams(q);return [p.get('app')||'1',p.get('g1')||'G0',p.get('g2')||'G0',p.get('n')||'1',p.get('m')||'1',p.get('view')||'disk',p.get('tile')||'1',p.get('bg')||'1'].join('|');
 }));
 for(let n=1;n<=23;n++)for(let m=1;m<=23;m++)for(const [view,tile,bg] of [['disk','1','1'],['disk','0','1'],['disk','1','0'],['disk','0','0'],['uhp','1','1']])
  assert.ok(family.has(['1','G0','G0',n,m,view,tile,bg].join('|')),`Missing N=${n},M=${m},${view},${tile},${bg}`);
 const files=[...manifest.map(([,f])=>f),...catalog.flatMap(r=>Array.from({length:5},(_,v)=>`static/tables/${r.n}-${v}.html.gz`))];
 const pictures=new Set();let next=0;
 await Promise.all(Array.from({length:12},async()=>{
  while(next<files.length){const file=files[next++],html=await read(file);
   assert.ok(!/<svg\b|data-part=/.test(html),file+' still contains SVG');
   if(file.includes('/tables/')) assert.ok(/class="static-plot"/.test(html),file+' has no picture');
   for(const m of html.matchAll(/static\/images\/[a-f0-9]+\.avif/g))pictures.add(m[0]);
  }
 }));
 const images=[...pictures];next=0;
 await Promise.all(Array.from({length:12},async()=>{
  while(next<images.length){const file=images[next++],m=await sharp(path.join(web,file)).metadata();
   assert.equal(m.format,'heif',file);assert.equal(m.compression,'av1',file);assert.equal(m.width,900,file);assert.equal(m.height,520,file);
  }
 }));
 const report=JSON.parse(await fs.readFile(path.join(dir,'coverage.json'),'utf8'));
 assert.equal(report.tableViews,full.length*5);assert.equal(report.families,manifest.length);
 console.log(`${files.length} shipped pages verified; ${full.length} table groups × 5 views; all 23×23 family pairs × 5 views; ${images.length} valid raster pictures; no SVG in shipped plots.`);
})().catch(e=>{console.error(e);process.exitCode=1});
