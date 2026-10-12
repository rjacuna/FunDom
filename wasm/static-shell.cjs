// Do not download the default Γ picture when the URL requests another group.
const prepare = html => {
 if(html.includes('data-initial-picture')) return html;
 const image=html.match(/<img class="static-plot"[^>]*>/)?.[0];
 if(!image) throw Error('Static shell has no default picture');
 const deferred=image.replace('class="static-plot"','class="static-plot" data-initial-picture')
  .replace(' src=',' data-src=').replace(' style=',' data-style=');
 const activate='<script>if(!location.search){const i=document.querySelector("[data-initial-picture]");i.src=i.dataset.src;if(i.dataset.style)i.setAttribute("style",i.dataset.style)}</script>';
 return html.replace(image,deferred+activate);
};
module.exports=prepare;
if(require.main===module){const fs=require('node:fs'),p=process.argv[2];fs.writeFileSync(p,prepare(fs.readFileSync(p,'utf8')));}
