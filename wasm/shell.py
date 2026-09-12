#!/usr/bin/env python3
# FunDom — Copyright (C) 2026 RJ Acuña. SPDX-License-Identifier: GPL-3.0-or-later
"""The page shell: the default page, pre-rendered by the native binary on
stdin, with the module script and the list of pre-rendered pages added
(and the CSS of the spinner the app shows while a page is computed).

    fundom page '' | python3 shell.py web/index.html web/pre/list.tsv
"""
import json, sys

html = sys.stdin.read()
if not html.strip():
    sys.exit("shell.py: no page on stdin")
if '<div id="plot">' not in html:
    sys.exit("shell.py: no plot area found in the page")
pre = []
if len(sys.argv) > 2:
    for line in open(sys.argv[2], encoding="utf-8"):
        line = line.rstrip("\n")
        if line:
            f, q = line.split("\t", 1)
            pre.append([q, f])
css = ('.plot-wait{display:flex;align-items:center;justify-content:center;max-width:100%;border:1px solid #d7d7de;'
       'border-radius:6px;background:#fff}.spin{width:44px;height:44px;border:4px solid #dfe3ee;border-top-color:#243b6b;'
       'border-radius:50%;animation:spin 1s linear infinite}@keyframes spin{to{transform:rotate(360deg)}}')
html = html.replace('</style>', css + '</style>', 1)
html = html.replace('</body>', '<script>window.fundomPre=' + json.dumps(pre, ensure_ascii=False).replace('</', '<\\/') + '</script>\n'
                    '<script type="module" src="./app.js"></script>\n'
                    '<noscript><p style="padding:12px">This page needs JavaScript: the mathematics runs in your browser.</p></noscript>\n</body>', 1)
open(sys.argv[1], 'w', encoding="utf-8").write(html)
print("wrote", sys.argv[1], len(html), "bytes,", len(pre), "pre-rendered pages listed")
