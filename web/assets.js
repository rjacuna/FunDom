const CACHE = "fundom-assets-v1";
const versions = {"csg.json": "6ba91d5ed8e16332d8230e2f9dd0903b50b83d8a396fdae7993a7f1681de0879", "fundom.wasm": "21dd9fc2dc9d3f793a1bf91b08474604cbc168b61082bf997c904fc3d62188e5"};

// Content-addressed downloads survive GitHub Pages' short HTTP cache lifetime.
// CacheStorage is optional: privacy mode/quota failures keep normal fetch working.
export async function asset(name, fingerprint = versions[name]) {
  const url = new URL(name, import.meta.url);
  if (fingerprint) url.searchParams.set("v", fingerprint);
  let cache;
  if (fingerprint && globalThis.caches) {
    try {
      cache = await caches.open(CACHE);
      const hit = await cache.match(url.href);
      if (hit) return hit;
    } catch { /* storage unavailable */ }
  }
  const response = await fetch(url);
  if (cache && response.ok) cache.put(url.href, response.clone()).catch(() => {});
  return response;
}
