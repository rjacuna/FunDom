// Build raster plots for every shipped family/example and every table group.
// NODE_PATH must contain sharp. Native geometry is unchanged; only delivery changes.
const fs = require('node:fs/promises'), path = require('node:path');
const {createHash} = require('node:crypto'), {gzipSync,gunzipSync} = require('node:zlib');
const {spawn, execFileSync} = require('node:child_process'), {createInterface} = require('node:readline');
const sharp = require('sharp');
sharp.concurrency(1); sharp.cache(false);
const root = path.resolve(__dirname, '..'), web = path.join(root, 'web');
const out = path.join(web, 'static');
const jobs = Number(process.env.FUNDOM_JOBS || 12);
const bin = process.env.FUNDOM_BIN;
const hash = x => createHash('sha256').update(x).digest('hex').slice(0, 24);
const pictureJobs = new Map();
const children = new Set();
const resume = process.env.FUNDOM_RESUME === '1';
const exists = file => fs.access(file).then(()=>true,()=>false);
let images = 0, pages = 0;
async function picture(svg, transparent=false) {
  // Links/titles differ between URLs but have no visual effect.
  svg = svg.replace(/<a\b[^>]*>|<\/a>|<title>[\s\S]*?<\/title>|<!--[\s\S]*?-->/g, '').replace(/>\s+</g,'><');
  const key = hash(svg + transparent), dest = `static/images/${key}.avif`;
  if (!pictureJobs.has(key)) pictureJobs.set(key, (async () => {
    try { await fs.access(path.join(web, dest)); }
    catch {
      const raster = sharp(Buffer.from(svg));
      const data = await (transparent ? raster : raster.flatten({background:'#fff'})).avif({quality:65,effort:2,chromaSubsampling:'4:4:4'}).toBuffer();
      await fs.writeFile(path.join(web, dest), data);
    }
    images++;
    return dest;
  })());
  return pictureJobs.get(key);
}
async function convert(html, dest, parts = false) {
  const match = html.match(/<svg\b[\s\S]*?<\/svg>/);
  if (match) {
    let svg = match[0];
    if (parts) {
      const first = (await fs.readFile(path.join(web,'pre/0.html'),'utf8')).match(/<g data-part="([^"]+)"><\/g>/);
      if (first) {
        const h = first[1], tess = await fs.readFile(path.join(web,'pre/parts',h+'.svg'),'utf8');
        svg = svg.replace(`<g data-part="${h}"></g>`, `<!--part:tess-->${tess}<!--/part-->`);
      }
      for (const m of svg.matchAll(/<g data-part="([^"]+)"><\/g>/g))
        svg = svg.replace(m[0], await fs.readFile(path.join(web, 'pre/parts', m[1]+'.svg'), 'utf8'));
    }
    // Final tiling/domain as one static raster; a second shared static raster
    // supplies the modular tessellation. This preserves alpha composition
    // and shares pictures between background-on and background-off views.
    let background;
    const disk = svg.includes('<clipPath id="disk">');
    if (disk) {
      const tess = svg.match(/<!--part:tess-->([\s\S]*?)<!--\/part-->/);
      if (tess && tess[1].trim()) {
        const header = svg.slice(0,svg.indexOf('<g id="layers"'));
        background = await picture(header + '<g clip-path="url(#disk)">' + tess[1] + '</g></svg>');
      }
      svg = svg.replace(/<!--part:tess-->[\s\S]*?<!--\/part-->/, '')
        .replace(/<rect[^>]*fill="#fff"\/>/, '')
        .replace(/<circle[^>]*stroke="none"\/>/, '');
    }
    const src = await picture(svg,disk);
    const w = svg.match(/\bwidth="(\d+)"/)[1], h = svg.match(/\bheight="(\d+)"/)[1];
    const note = (svg.match(/<text\b[^>]*>([^<]*)<\/text>/)||[])[1] || 'Fundamental domain';
    html = html.replace(match[0], `<img class="static-plot" src="./${src}" width="${w}" height="${h}" fetchpriority="high" decoding="async" ${background ? `style="background-image:url(./${background});background-size:100% 100%" data-background="${background}"` : ''} alt="${note}">`);
    const explore = new URLSearchParams((html.match(/href="svg\?([^"]*)"/)||[])[1]?.replaceAll('&amp;', '&') || '');
    explore.set('interactive','1');
    html = html.replace('<div id="plot">', `<div id="plot"><a class="btn" style="display:inline-block;margin-bottom:6px" href="?${explore.toString().replaceAll('&','&amp;')}">Explore triangles</a>`);
    html = html.replace('</style>', '.static-plot{display:block;max-width:100%;height:auto;border:1px solid #d7d7de;border-radius:6px;background:white}</style>');
    html = html.replace('Click a triangle for its matrix. In edit mode, click a yellow marker to redraw the triangle paired with that side there.', 'Choose triangle explorer for individual triangles and matrices, or edit mode to change side pairings.');
    // The standard half-plane button should auto-fit, not inherit an irrelevant disk zoom.
    if (disk) {
      html = html.replace(/href="\?([^"]*)"/g, (all,q) => {
        const p = new URLSearchParams(q.replaceAll('&amp;', '&'));
        if (p.get('view') === 'uhp' && /modular tessellation/.test(html)) {
          p.delete('scale');p.delete('cx');return `href="?${p.toString().replaceAll('&','&amp;')}"`;
        }
        return all;
      });
    }
  }
  // The persistent shell already has the stylesheet and math loader. Do not
  // repeat them in forty thousand fragments.
  const body=html.match(/<body>([\s\S]*)<\/body>/)[1];
  await fs.writeFile(path.join(web,dest), gzipSync('<!doctype html><title>Fundamental domains</title><body>'+body+'</body>',{level:9,mtime:0}));
  pages++;
  return html;
}
const compact = r => [r.n,r.l,r.i,r.g,r.m,r.mg.map(m=>m.join(',')).join(';'),r.cu.join(' '),r.c2.join(' '),r.c3.join(' '),r.s,r.con,r.len,r.gal.join(' '),r.sup.join(' '),r.sub.join(' ')].join('|');
async function tableWorker(records, id) {
  const dir = path.join(out, `.worker-${id}`);await fs.mkdir(dir,{recursive:true});
  const child = spawn(bin,['static-tables',dir],{cwd:root,stdio:['pipe','pipe','inherit']});
  children.add(child);child.on('exit',()=>children.delete(child));
  const lines = createInterface({input:child.stdout})[Symbol.asyncIterator]();
  let error; child.on('error',e=>error=e);
  const exit = new Promise(resolve=>child.on('exit',code=>resolve(code)));
  for (const r of records) {
    if (resume && (await Promise.all(Array.from({length:5},(_,v)=>exists(path.join(out,'tables',`${r.n}-${v}.html.gz`))))).every(Boolean)) {pages+=5;continue;}
    if (error) throw error;
    child.stdin.write(compact(r)+'\n');
    const ack = await lines.next();
    if (ack.done || ack.value !== r.n) throw Error(`Native table renderer stopped at ${r.n}`);
    for(let v=0;v<5;v++) {
      const file=path.join(dir,`${r.n}-${v}.html`);
      await convert(await fs.readFile(file,'utf8'),`static/tables/${r.n}-${v}.html.gz`);
      await fs.unlink(file);
    }
  }
  child.stdin.end();
  if (await exit !== 0) throw Error('Native table renderer failed');
  await fs.rm(dir,{recursive:true});
}
(async()=>{
  if(!bin) throw Error('Set FUNDOM_BIN to the native fundom binary');
  if(!resume) await fs.rm(out,{recursive:true,force:true});
  for(const dir of ['images','pages','tables']) await fs.mkdir(path.join(out,dir),{recursive:true});
  const list=(await fs.readFile(path.join(web,'pre/list.tsv'),'utf8')).trimEnd().split('\n').map(l=>{const i=l.indexOf('\t');return [l.slice(i+1),l.slice(0,i)]});
  const manifest=[];
  let next=0;
  await Promise.all(Array.from({length:jobs},async()=>{
    while(next<list.length) {
      const i=next++, [q,file]=list[i], dest=`static/pages/${i}.html.gz`;
      if(resume && await exists(path.join(web,dest))) pages++;
      else await convert(await fs.readFile(path.join(web,file),'utf8'),dest,true);
      manifest[i]=[q,dest];
    }
  }));
  const records=JSON.parse(await fs.readFile(path.join(web,'csg.json'),'utf8'));
  await fs.writeFile(path.join(out,'catalog.json.gz'),gzipSync(JSON.stringify(records.map(({n,g,l,i,s})=>({n,g,l,i,s}))),{level:9}));
  await fs.writeFile(path.join(out,'manifest.json.gz'),gzipSync(JSON.stringify(manifest),{level:9}));
  execFileSync('python3',[path.join(__dirname,'shell.py'),path.join(web,'index.html'),path.join(web,'pre/list.tsv')],{input:await fs.readFile(path.join(web,'pre/0.html'))});
  let index=await fs.readFile(path.join(web,'index.html'),'utf8');
  index=await convert(index,'static/default.html.gz',true);
  index=index.replace(/window\.fundomPre=[\s\S]*?<\/script>/,'window.fundomPre='+JSON.stringify(manifest).replaceAll('</','<\\/')+'</script>');
  index=index.replace('app.js?v=background-1','app.js?v=static-1');
  await fs.writeFile(path.join(web,'index.html'),index);
  console.log(`Family views ready: ${pages} pages, ${images} distinct pictures. Building ${records.length} table groups.`);
  const progress=setInterval(()=>console.log(`${pages} pages / ${images} distinct pictures`),30000);
  try {await Promise.all(Array.from({length:jobs},(_,i)=>tableWorker(records.filter((_,k)=>k%jobs===i),i)));}
  finally {clearInterval(progress);}
  if(resume) for(const dir of ['pages','tables']) for(const file of await fs.readdir(path.join(out,dir))) {
    const dest=path.join(out,dir,file), html=gunzipSync(await fs.readFile(dest)).toString();
    if(html.includes('<head>')) {
      const body=html.match(/<body>([\s\S]*)<\/body>/)[1];
      await fs.writeFile(dest,gzipSync('<!doctype html><title>Fundamental domains</title><body>'+body+'</body>',{level:9}));
    }
  }
  // Old SVG parts and HTML are build intermediates. Interactive views are computed by WASM.
  await fs.rm(path.join(web,'pre'),{recursive:true});
  images=(await fs.readdir(path.join(out,'images'))).length;
  const report={families:list.length,tableGroups:records.length,tableViews:records.length*5,pages,images};
  await fs.writeFile(path.join(out,'coverage.json'),JSON.stringify(report,null,2)+'\n');
  console.log(report);
})().catch(e=>{for(const child of children) child.kill();console.error(e);process.exitCode=1});
