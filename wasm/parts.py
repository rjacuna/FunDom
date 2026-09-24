#!/usr/bin/env python3
# FunDom — Copyright (C) 2026 RJ Acuña. SPDX-License-Identifier: GPL-3.0-or-later
"""Split the large layers out of pre-rendered pages, into files served on their own.

    python3 wasm/parts.py web/pre/parts web/pre/*.html web/index.html

A page drawn in the disk carries its two large layers -- the modular tessellation and the tiling by the
Γ-translates, a megabyte or two of path data between them -- between <!--part:NAME--> and <!--/part-->.  Each such
run is written once to PARTS/<hash>.svg, named by its content, and replaced in the page by an empty
<g data-part="<hash>"></g> that web/app.js fills.  The tessellation is the same on every page, and the tiling is the
same on every page that draws the same group in the same colours (both orders of Γ₀(N) ∩ Γ₀(M), the five views of
it that tile, the ones whose intersection is the same group), so each is one file the browser fetches once and
keeps; what is left of a page is its panels and the domain, tens of kilobytes.  A run smaller than INLINE bytes is
not worth a request and stays in the page.  The markers go either way.
"""
import hashlib, os, re, sys

INLINE = 16384
RUN = re.compile(r"<!--part:[A-Za-z0-9_-]*-->(.*?)<!--/part-->", re.S)

if len(sys.argv) < 3:
    sys.exit(__doc__)
parts_dir, files = sys.argv[1], sys.argv[2:]
os.makedirs(parts_dir, exist_ok=True)
written, shared, inlined, before, after = set(), 0, 0, 0, 0

def split(m):
    global shared, inlined
    body = m.group(1)
    data = body.encode("utf-8")
    if len(data) < INLINE:
        inlined += 1
        return body
    h = hashlib.sha1(data).hexdigest()[:20]
    path = os.path.join(parts_dir, h + ".svg")
    if h in written or os.path.exists(path):
        shared += 1
    else:
        with open(path, "wb") as f:
            f.write(data)
    written.add(h)
    return '<g data-part="' + h + '"></g>'

for name in files:
    text = open(name, encoding="utf-8").read()
    before += len(text.encode("utf-8"))
    text = RUN.sub(split, text)
    after += len(text.encode("utf-8"))
    open(name, "w", encoding="utf-8").write(text)

size = sum(os.path.getsize(os.path.join(parts_dir, f)) for f in os.listdir(parts_dir) if f.endswith(".svg"))
print(f"{len(files)} pages, {before / 1e6:.1f} MB -> {after / 1e6:.1f} MB; "
      f"{len(written)} parts ({size / 1e6:.1f} MB in {parts_dir}), {shared} uses of a part already written, {inlined} small runs kept inline")
