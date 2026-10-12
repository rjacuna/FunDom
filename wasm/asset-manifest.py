#!/usr/bin/env python3
"""Refresh browser cache keys after rebuilding the WebAssembly assets."""
from pathlib import Path
import hashlib, json
web = Path(__file__).resolve().parent.parent / "web"
files = ['fundom.wasm', 'csg.json']
versions = {f: hashlib.sha256((web / f).read_bytes()).hexdigest() for f in files if (web / f).exists()}
loader = web / "assets.js"
text = loader.read_text()
start = text.index("const versions = ")
end = text.index(";", start) + 1
loader.write_text(text[:start] + "const versions = " + json.dumps(versions, sort_keys=True) + ";" + text[end:])
