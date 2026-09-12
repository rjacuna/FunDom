// From CriticalValues (Tomorrow's Talk/CriticalValues/haskell/web/js/wasi-shim.js), unchanged.
// A minimal WASI shim, enough for a GHC wasm32-wasi reactor module that does
// no real I/O.
//
// Rather than depend on an npm package, this introspects the module's declared
// imports and supplies every `wasi_snapshot_preview1` function it asks for:
// the handful that matter are implemented, the rest return 0 (WASI success).
// If GHC's RTS ever starts needing one of the stubs for real, it will show up
// as wrong behaviour rather than an instantiation failure — so the stubs log.
export function wasiImports(module, { onStdout = (s) => console.log(s) } = {}) {
  let memory = null;
  const dec = new TextDecoder();
  const setMemory = (m) => { memory = m; };

  const real = {
    // Haskell writes to stderr on panic; surface it rather than swallow it.
    fd_write(fd, iovs, iovsLen, nwritten) {
      const view = new DataView(memory.buffer);
      let written = 0, text = "";
      for (let i = 0; i < iovsLen; i++) {
        const ptr = view.getUint32(iovs + i * 8, true);
        const len = view.getUint32(iovs + i * 8 + 4, true);
        text += dec.decode(new Uint8Array(memory.buffer, ptr, len));
        written += len;
      }
      view.setUint32(nwritten, written, true);
      if (text) onStdout(text);
      return 0;
    },
    clock_time_get(_id, _prec, out) {
      new DataView(memory.buffer).setBigUint64(out, BigInt(Date.now()) * 1000000n, true);
      return 0;
    },
    random_get(ptr, len) {
      crypto.getRandomValues(new Uint8Array(memory.buffer, ptr, len));
      return 0;
    },
    proc_exit(code) { throw new Error("proc_exit(" + code + ")"); },
    // The construction opens no files and reads no arguments.
    args_get() { return 0; },          // no argv; the export takes its input directly
    environ_get() { return 0; },
    args_sizes_get(argc, argvBufSize) {
      const v = new DataView(memory.buffer);
      v.setUint32(argc, 0, true); v.setUint32(argvBufSize, 0, true); return 0;
    },
    environ_sizes_get(c, bufSize) {
      const v = new DataView(memory.buffer);
      v.setUint32(c, 0, true); v.setUint32(bufSize, 0, true); return 0;
    },
  };

  const wasi = {};
  for (const imp of WebAssembly.Module.imports(module)) {
    if (imp.module !== "wasi_snapshot_preview1" || imp.kind !== "function") continue;
    wasi[imp.name] = real[imp.name] ?? ((...a) => {
      console.warn("WASI stub called:", imp.name, a);
      return 0;
    });
  }
  return { wasi, setMemory };
}
