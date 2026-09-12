#!/usr/bin/env python3
# FunDom — Copyright (C) 2026 RJ Acuña. SPDX-License-Identifier: GPL-3.0-or-later
"""The page shell: the default page, pre-rendered by the native binary on
stdin, with a spinner where the picture goes and the module script added.

    fundom page '' | python3 shell.py web/index.html
"""
import re, sys

html = sys.stdin.read()
if not html.strip():
    sys.exit("shell.py: no page on stdin")
spin = '<div class="plot-wait" style="width:900px;height:520px"><div class="spin"></div></div>'
html, n = re.subn(r'<div id="plot">.*?</div>(?=\n<div class="panel">)', '<div id="plot">' + spin + '</div>', html, count=1, flags=re.S)
if n != 1:
    sys.exit("shell.py: no plot area found in the page")
css = ('.plot-wait{display:flex;align-items:center;justify-content:center;max-width:100%;border:1px solid #d7d7de;'
       'border-radius:6px;background:#fff}.spin{width:44px;height:44px;border:4px solid #dfe3ee;border-top-color:#243b6b;'
       'border-radius:50%;animation:spin 1s linear infinite}@keyframes spin{to{transform:rotate(360deg)}}')
html = html.replace('</style>', css + '</style>', 1)
html = html.replace('</body>', '<script type="module" src="./app.js"></script>\n'
                    '<noscript><p style="padding:12px">This page needs JavaScript: the mathematics runs in your browser.</p></noscript>\n</body>', 1)
open(sys.argv[1], 'w').write(html)
print("wrote", sys.argv[1], len(html), "bytes")
