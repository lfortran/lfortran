# LFortran in the Browser (JupyterLite)

LFortran runs entirely inside the browser at

[https://lfortran.github.io/lfortran/lab/index.html](https://lfortran.github.io/lfortran/lab/index.html)

(linked from the `README.md` badge). There is no server: the whole compiler is
compiled to WebAssembly and executed by the browser through the `xlfortran`
[xeus-lite](https://github.com/jupyter-xeus/xeus-lite) kernel inside
[JupyterLite](https://jupyterlite.readthedocs.io/).

This page explains how to build and run that site locally, so that bugs seen in
the online lab can be reproduced and fixed without going through CI, and how to
turn such a bug into a test. The short version, on Linux or macOS:

```bash
pixi run lab   # builds the WASM kernel and the site, then serves it at
               # http://localhost:8000/lab/index.html
```

## How the deployed site is produced

The site is built and deployed by the `build_xlfortran_jupyterlite` and
`deploy_jupyterlite` jobs in `.github/workflows/Exhaustive-Checks-CI.yml`.
The jobs run on every push to `main` (and on PRs labelled
`Tests::Run-Exhaustive`) and run the same scripts the pixi tasks below wrap:

| CI step | Local equivalent |
| --- | --- |
| `./build0.sh` — generate the parser / AST / ASR headers | (run automatically) |
| `./wasm-build0.sh` — build a *native* `lfortran` and compile the runtime `.mod` files into the WASM *host* environment | `pixi run wasm-mods` |
| `./wasm-build1.sh` — cross-compile LFortran and the `xlfortran` kernel to WebAssembly with Emscripten | `pixi run wasm-kernel` |
| `node build-wasm/src/lfortran/tests/test_lfortran.js` — evaluator test-suite inside the WASM runtime | `pixi run wasm-test` |
| `jupyter lite build` — assemble the site (`dist/`) with the kernel and the demo notebooks from `share/lfortran/nb/` | `pixi run lab-build` |
| Deploy `dist/` to GitHub Pages | `pixi run lab` (serves it locally instead) |

## Three ways to reproduce a lab bug locally

Pick the cheapest one that still shows the bug:

| Approach | Build cost | Use when |
| --- | --- | --- |
| C++ evaluator test (`FortranEvaluator::evaluate2`) | native build only | Almost always — this is also the form the *fix* has to be tested in. See "Writing a test" below. |
| Native Jupyter kernel (`-DWITH_XEUS=yes`) + JupyterLab | native build only | You need real notebook behaviour: rich display (`display_data`), streams, cell-by-cell state, error rendering. |
| Full JupyterLite/WASM build (`pixi run lab`) | ~30–60 min the first time | The bug is WASM-specific, or you want to confirm the fix in exactly the deployed artifact. |

The kernel logic is shared by all three: `src/lfortran/fortran_kernel.cpp`
drives `FortranEvaluator` (`src/lfortran/fortran_evaluator.cpp`), and each
notebook cell is one call to `FortranEvaluator::evaluate2()`. Only the transport
(ZeroMQ vs. `xeus-lite` in a web worker) and the code generator target differ.
So a bug that is not about WASM itself will reproduce in the native kernel and,
usually, in a plain C++ evaluator test.

## Option A — native Jupyter kernel (fast loop)

This is the ordinary xeus kernel, described in
[Enabling the Jupyter Kernel](installation.md). In short:

```bash
conda install xeus>=6.0.0 xeus-zmq>=4.0.0 nlohmann_json jupyter -c conda-forge

cmake -S . -B build -G Ninja \
    -DCMAKE_BUILD_TYPE=Debug \
    -DWITH_LLVM=yes \
    -DWITH_XEUS=yes \
    -DCMAKE_PREFIX_PATH="$CONDA_PREFIX" \
    -DCMAKE_INSTALL_PREFIX="$CONDA_PREFIX"
cmake --build build -j --target install

jupyter kernelspec list --json   # "fortran" must be listed
jupyter lab share/lfortran/nb/Mandelbrot.ipynb
```

Open one of the notebooks under `share/lfortran/nb/` — these are the same
notebooks that ship with the online lab (`Demo1`, `Demo2`, `Variables`,
`Mandelbrot`, `Rich_Display`) — and run the failing cell. Rich output
(`use lfortran_display; call display_data(...)`) works here, because the
`lfortran_display_data` / `lfortran_clear_output` bridge symbols are defined in
the kernel binary and resolved by the JIT.

Note that the command-line REPL (`lfortran` with no arguments) is **not** a
substitute: it is line-oriented, so a multi-line cell (in particular one
containing a `module ... end module`) is evaluated as a sequence of separate
`evaluate2()` calls rather than as one cell, which changes the behaviour.

## Option B — full JupyterLite build

Everything is driven by [pixi](https://pixi.sh) tasks defined in `pixi.toml`:

```bash
pixi run wasm-kernel   # cross-compile LFortran + the xlfortran kernel to WASM
pixi run lab           # assemble the site and serve it
```

`lab` depends on `wasm-kernel`, so `pixi run lab` on its own builds everything
it needs. Then open <http://localhost:8000/lab/index.html> — the local
equivalent of the deployed URL. A plain `file://` open does **not** work: the
kernel is loaded by a web worker and needs a real HTTP origin.

Pixi creates the required environments on first use; expect ~30–60 min and
several GB for the first full build (`dist/` alone is about 250 MB). The build
tasks declare `inputs`/`outputs`, so re-running `pixi run lab` with nothing
changed is a cache hit and starts serving in under a second; edit a source file
and only the affected steps re-run.

Supported on Linux and macOS — the Emscripten cross toolchain is not published
for Windows. Verified end-to-end on macOS (arm64) and, in CI, on Linux.

### The tasks

| Task | What it does |
| --- | --- |
| `pixi run wasm-host-env` | Installs the `wasm-host` environment (see below). Pulled in automatically. |
| `pixi run wasm-mods` | `wasm-build0.sh`: builds a native `lfortran` and compiles the runtime `.mod` files into the `wasm-host` environment. |
| `pixi run wasm-kernel` | `wasm-build1.sh`: cross-compiles LFortran and `xlfortran` to WebAssembly into `build-wasm/`. |
| `pixi run wasm-test` | Runs the evaluator test-suite inside the WASM runtime under `node`, exactly as CI does. |
| `pixi run lab-build` | `jupyter lite build`: assembles the site into `dist/`, embedding the kernel and the demo notebooks. |
| `pixi run lab` | Serves `dist/` at <http://localhost:8000/lab/index.html>. |

Each task depends on the previous ones, so any of them can be invoked directly.

Two things about these tasks are worth knowing when editing `pixi.toml`:

* Task `outputs` must live outside `.pixi/`, which pixi's glob walker skips —
  that is why `wasm-kernel` declares `build-wasm/xlfortran.*` rather than the
  installed copies in the `wasm-host` environment.
* `lab-build` passes `--XeusAddon.default_channels` explicitly. `jupyterlite-xeus`
  otherwise recovers the channel list from the prefix's `conda-meta/history`,
  which micromamba writes and pixi does not; without it the build fails late
  with `Cannot detect channels from prefix ...`. The flag must be **repeated
  once per channel** — passing a quoted list (`="['a','b']"`) makes traitlets
  store the bracketed text as a single channel, which builds without error but
  produces a site whose kernel fails to start.

### The environments

Three pixi environments back these tasks:

| Environment | Purpose |
| --- | --- |
| `wasm-build` | native tools (python, cmake, re2c, bison) plus the Emscripten toolchain (`emcmake`, `emmake`, `node`) |
| `wasm-host` | WASM *target* libraries — `llvm`, `xeus`, `xeus-lite`, `nlohmann_json` — plus the runtime `.mod` files |
| `lite` | `jupyterlite-xeus` + `jupyter_server`, to assemble and serve the site |

`wasm-host` targets the `emscripten-wasm32` platform, which the host machine
cannot execute, so pixi only installs it when that platform is named
explicitly. That is what the `wasm-host-env` task does:

```bash
pixi install -e wasm-host --platform emscripten-wasm32
```

Its directory, `.pixi/envs/wasm-host`, is what the build scripts see as
`$PREFIX`; the tasks set that variable for you.

Because that environment adds `emscripten-wasm32` to the workspace, every other
feature in `pixi.toml` declares an explicit `platforms` list — an environment is
only solved for the platforms shared by all of its features, and packages like
`python` do not exist for `emscripten-wasm32`.

### What each step actually does

1. **`wasm-mods` / `wasm-build0.sh`** builds a throwaway native `lfortran` (in
   `asset_dir/`, without LLVM), compiles `src/runtime/**/*.f90` with it, and
   copies the resulting `.mod` files into `$PREFIX/lib/`. They are preloaded
   into the WASM virtual filesystem at `/lib/` at link time, which is how
   `use iso_c_binding` and `use lfortran_display` resolve in the browser.
   Re-run after any change under `src/runtime/`:

   ```bash
   pixi run wasm-mods
   ls .pixi/envs/wasm-host/lib/*.mod
   ```

2. **`wasm-kernel` / `wasm-build1.sh`** configures `build-wasm/` via `emcmake`
   with `-DXEUS_LFORTRAN_WASM_BUILD=yes`, builds `xlfortran.js`,
   `xlfortran.wasm` and `xlfortran.data` there and installs them into
   `$PREFIX/bin/`, alongside the
   kernelspec in `$PREFIX/share/jupyter/kernels/fortran/` (generated from
   `share/jupyter/kernels/fortran/wasm_kernel.json.in`). This is the slow step
   (it links LLVM); incremental rebuilds after a source edit are much faster.

3. **`lab-build` / `jupyter lite build`** assembles `dist/` from the kernel in
   the `wasm-host` environment plus the demo notebooks in `share/lfortran/nb/`
   (`Demo1`, `Demo2`, `Variables`, `Mandelbrot`, `Rich_Display`) — the same set
   the deployed site ships. To add your own reproducer notebook, add another
   `--contents` flag to the `lab-build` task in `pixi.toml`.

### Edit → rebuild loop

After changing LFortran sources:

```bash
pixi run lab       # rebuilds the kernel and the site, then serves it
```

**Then clear the browser state — a reload is not enough.** JupyterLite installs
a service worker that serves the app itself from cache, and `jupyterlite-xeus`
keeps the unpacked kernel packages in IndexedDB. Both survive a rebuild *and* a
hard reload, so after replacing `dist/` the browser can keep running the
previous build — including a previously broken one, which looks exactly like
"my fix did nothing".

The reliable options, in order of convenience:

* Open the site in a **private/incognito window** — fresh state every time.
  This is the sanest default while iterating.
* Clear the origin's storage: in Firefox click the **padlock** in the URL bar →
  *Clear cookies and site data…*; in Chrome use DevTools → Application →
  Storage → *Clear site data*. Both drop the service worker, IndexedDB and
  local storage in one step. Note this also discards notebooks you edited
  inside the lab, which live in browser storage.
* Serve on another port (`python3 -m http.server -d dist 8001`). A different
  port is a different origin, so there is no service worker and no cached app.

A hard reload (`Cmd`/`Ctrl`+`Shift`+`R`) is *not* sufficient on its own: it
bypasses the HTTP cache, but the service worker still answers first.

A quick way to tell whether you are looking at cached content: watch the
`python -m http.server` log while you load the page. A genuinely fresh load
requests `/lab/index.html`, `/build/lab/bundle.js` and a long list of
`/build/*.js` and `/extensions/...` files. If you only see `jupyter-lite.json`
and the `xeus/` kernel assets, the app came from the service worker.

Notebook *contents* are likewise stored in browser storage, so an edited
notebook keeps its locally-stored version even after a rebuild — use
"File → Reset" or clear storage to pick up a new `--contents` version.

### Troubleshooting

* The site behaves as it did before your rebuild (an old bug is still there, a
  fix has no effect, the kernel still fails to start) — almost always the
  service worker / IndexedDB cache described above. Retry in a private window
  before debugging anything else.
* To check whether the problem is the build or the browser, serve `dist/` and
  load it in a fresh browser profile. If the kernel works there, `dist/` is
  fine and the browser state is stale.
* The kernel indicator spins at *Connecting* forever and cells stay at `[*]`,
  in one browser profile only — a browser extension or a hardening preference
  is blocking the kernel's web worker. This survives clearing site data and
  changing ports, and a private window does not help because extensions may
  run there too. Confirm with a brand-new profile (`about:profiles` in Firefox,
  a new profile directory in Chrome); if that works, bisect with Firefox's
  *Help → Troubleshoot Mode* (extensions off, preferences kept). Script
  blockers such as NoScript or JShelter, and `privacy.resistFingerprinting`,
  are the usual causes. Note that wasm itself may still test fine — the block
  is on the worker, not on WebAssembly.
* `no runtime .mod files found in .../lib` — run `pixi run wasm-mods`.
* `does not support 'osx-arm64' on this machine` for `wasm-host` — install it
  with `pixi install -e wasm-host --platform emscripten-wasm32` (the
  `wasm-host-env` task).
* `Module 'x' modfile was not found` in the browser but not natively — the
  module's `.mod` file was not preloaded; check `.pixi/envs/wasm-host/lib/*.mod`
  and rebuild with `pixi run wasm-kernel`.
* Blank page or "kernel failed to start" — check the browser console; usually a
  missing `xlfortran.data`, meaning `lab-build` ran against a stale kernel.
  Re-run `pixi run wasm-kernel` then `pixi run lab`.
* `Failed to detect channel from <url> with known channels ...` in the browser
  console, followed by the kernel never becoming ready — the channel list in
  `dist/xeus/wasm-host/empack_env_meta.json` does not match the channels the
  packages came from. Check that file: `channels` must be a list of plain URLs.
* `Cannot detect channels from prefix ...` — the `--XeusAddon.default_channels`
  argument was dropped from the `lab-build` task; see above.
* A task is skipped when you expected it to run — its `inputs` did not change.
  Touch the relevant file, or delete the task's `outputs` (for example
  `rm -rf build-wasm/xlfortran.*`), and re-run.
* `Incompatible format: LFortran Modfile` in the browser — the preloaded `.mod`
  files predate a modfile-format change, and `wasm-mods` only watches
  `src/runtime/`. Force it: `rm -rf .pixi/envs/wasm-host/lib/*.mod` and run
  `pixi run wasm-mods` after touching `wasm-build0.sh`, or just
  `rm -rf .pixi/envs/wasm-host` and rebuild.
* To start over: `rm -rf build-wasm dist .pixi/envs/wasm-host`.

### Running the same commands as CI

CI does not use pixi; it drives the same three scripts with micromamba
environments built from `ci/environment_linux.yml`, `environment-wasm-build.yml`
and `environment-wasm-host.yml` (see `.github/workflows/Exhaustive-Checks-CI.yml`).
The scripts are identical — only the environment manager and the value of
`PREFIX` differ:

```bash
export PREFIX=$MAMBA_ROOT_PREFIX/envs/xeus-lfortran-wasm-host
micromamba run -n lf ./wasm-build0.sh
micromamba run -n xeus-lfortran-wasm-build ./wasm-build1.sh
```

## Writing a test for a lab bug

Follow the general rules in `AGENTS.md` / `CLAUDE.md`: one bug = one MRE = one
PR, and the test must fail before the fix and pass after it.

### Step 1 — decide whether the bug is interactive-mode specific

Rewrite the failing notebook cells as a single ordinary Fortran program and
compile it with `lfortran` (and with `gfortran` for a reference):

```bash
lfortran mre.f90 && ./a.out
```

* If it fails as a program too, the bug has nothing to do with the notebook.
  Write an ordinary **integration test** in `integration_tests/` and register it
  in `integration_tests/CMakeLists.txt` with the `gfortran` and `llvm` labels.
  That is the preferred form of every LFortran test.
* If it only fails when the code is split across cells (state persisting in the
  global symbol table between cells, modules defined in one cell and used in the
  next, a symbol re-added on a second evaluation, ...), it is an *interactive
  mode* bug and needs an evaluator test — continue below.

### Step 2 — write the MRE as an evaluator test

One notebook cell == one `evaluate2()` call. Add a `doctest` `TEST_CASE` to
`src/lfortran/tests/test_llvm.cpp`, next to the other `FortranEvaluator` cases:

```cpp
TEST_CASE("FortranEvaluator module defined in one cell, used in the next") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    // cell 1
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2(
        "module m\n"
        "implicit none\n"
        "contains\n"
        "    integer function f()\n"
        "        f = 5\n"
        "    end function\n"
        "end module\n");
    CHECK(r.ok);

    // cell 2
    r = e.evaluate2("use m\ninteger :: i\ni = f()\n");
    CHECK(r.ok);

    // cell 3
    r = e.evaluate2("i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 5);
}
```

Points to get right:

* `cu.interactive = true` — without it the evaluator does not keep the global
  symbol table across calls, and the bug will not reproduce.
* `cu.po.runtime_library_dir = ...` — needed for any cell that does `use` of a
  runtime module.
* Keep the cell split of the original notebook. Merging cells that the notebook
  kept separate (or splitting a cell the notebook kept together) can make the
  bug disappear — that split *is* the reproducer.
* Check the value, not only `r.ok`. `EvalResult::type` is one of
  `integer4/integer8/real4/real8/complex4/complex8/boolean/statement/none`; a
  bare expression cell returns the typed value, a statement cell returns
  `statement`, and a declaration or `use` returns `none`. A cell that silently
  produces nothing usually shows up here as `statement`/`none` where a value was
  expected, or as `r.ok == false`.

### Step 3 — reduce

Delete lines and merge/simplify cells until every remaining line is needed to
make the `CHECK` fail. The `create-mre` skill (`.agents/skills/create-mre/`)
describes the reduction procedure in detail; it applies unchanged here, with the
`.f90` + `run.sh` reproducer replaced by the `TEST_CASE` above.

### Step 4 — run it

Natively:

```bash
cmake --build build -j --target test_lfortran
./build/src/lfortran/tests/test_lfortran -tc="FortranEvaluator module*"
```

(or `ctest -R test_lfortran` from `build/` to run the whole suite).

Inside WASM:

```bash
pixi run wasm-test
```

That rebuilds the kernel first. For a tighter loop, rebuild and run only the
test binary — `xlfortran` itself does not need relinking until you rebuild the
site:

```bash
pixi run -e wasm-build emmake make -C build-wasm test_lfortran -j8
(cd build-wasm/src/lfortran/tests && \
    pixi run -e wasm-build node test_lfortran.js -tc="FortranEvaluator module*")
```

For the JupyterLite build, `src/lfortran/tests/CMakeLists.txt` reduces the test
sources to `test_llvm.cpp` only, so every test you add there is automatically
part of the WASM run.

If a test only makes sense in one of the two worlds, guard it:

```cpp
#ifdef __EMSCRIPTEN__      // WASM-only (e.g. preloaded .mod resolution)
#ifndef __EMSCRIPTEN__     // native-only (e.g. large stack allocations)
```

### A note on rich display (`display_data`)

Bugs of the form "the cell should show an image but shows nothing" cannot be
asserted end-to-end in an evaluator test: `lfortran_display_data()` is defined
in the kernel (`src/lfortran/fortran_kernel.cpp`), not in the test binary, so a
test that actually calls it will not link/resolve. Reduce such a bug to the
compile/evaluate failure underneath it instead:

1. In the notebook, replace `call display_data(...)` with
   `print *, len(payload)` (or print a checksum of the payload). If the cell now
   prints nothing or errors, the display bridge is innocent and you have a plain
   evaluator bug — write the test as in step 2.
2. If the payload is computed correctly and only the display is missing, the bug
   is in the kernel/display path. Reproduce it with Option A (native kernel),
   which exercises the same `lfortran_display_data` code path with a debugger
   attached, and keep a notebook-level reproducer under `share/lfortran/nb/`
   plus an evaluator test for whatever compile-time part can be asserted.
