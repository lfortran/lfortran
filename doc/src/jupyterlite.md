# LFortran in the Browser (JupyterLite)

LFortran runs entirely inside the browser at

[https://lfortran.github.io/lfortran/lab/index.html](https://lfortran.github.io/lfortran/lab/index.html)

(linked from the `README.md` badge). There is no server: the whole compiler is
compiled to WebAssembly and executed by the browser through the `xlfortran`
[xeus-lite](https://github.com/jupyter-xeus/xeus-lite) kernel inside
[JupyterLite](https://jupyterlite.readthedocs.io/).

This page explains how to build and run that site locally, so that bugs seen in
the online lab can be reproduced and fixed without going through CI, and how to
turn such a bug into a test.

## How the deployed site is produced

The site is built and deployed by the `build_xlfortran_jupyterlite` and
`deploy_jupyterlite` jobs in `.github/workflows/Exhaustive-Checks-CI.yml`.
The jobs run on every push to `main` (and on PRs labelled
`Tests::Run-Exhaustive`) and do exactly the steps documented below:

1. `./build0.sh` — generate the parser / AST / ASR headers.
2. `./wasm-build0.sh` — build a *native* `lfortran` and use it to compile the
   runtime `.mod` files, which are installed into the WASM *host* environment.
3. `./wasm-build1.sh` — cross-compile LFortran and the `xlfortran` kernel to
   WebAssembly with Emscripten.
4. `node build-wasm/src/lfortran/tests/test_lfortran.js` — run the evaluator
   test-suite inside the WASM runtime.
5. `jupyter lite build` — assemble the JupyterLite site (`dist/`), embedding the
   kernel and the demo notebooks from `share/lfortran/nb/`.
6. Deploy `dist/` to GitHub Pages.

## Three ways to reproduce a lab bug locally

Pick the cheapest one that still shows the bug:

| Approach | Build cost | Use when |
| --- | --- | --- |
| C++ evaluator test (`FortranEvaluator::evaluate2`) | native build only | Almost always — this is also the form the *fix* has to be tested in. See "Writing a test" below. |
| Native Jupyter kernel (`-DWITH_XEUS=yes`) + JupyterLab | native build only | You need real notebook behaviour: rich display (`display_data`), streams, cell-by-cell state, error rendering. |
| Full JupyterLite/WASM build | ~30–60 min the first time | The bug is WASM-specific, or you want to confirm the fix in exactly the deployed artifact. |

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

All commands are run **from the repository root** (`jupyter lite build` resolves
its `--contents` paths relative to the current directory). Uses
[micromamba](https://mamba.readthedocs.io/en/latest/user_guide/micromamba.html),
like CI; `conda`/`mamba` work the same way. If you do not have it:
`pixi global install micromamba` (or the official install script).

These steps were run end-to-end on macOS (arm64, Darwin 24.6) as well as on
Linux in CI. Expect ~30–60 min and ~10 GB for the first full build; `dist/`
alone is about 250 MB.

### Quick reference

Once the environments of step 0 exist, the whole loop is:

```bash
export MAMBA_ROOT_PREFIX=$HOME/micromamba
export PREFIX=$MAMBA_ROOT_PREFIX/envs/xeus-lfortran-wasm-host

micromamba run -n lf ./build0.sh                              # only after grammar changes
micromamba run -n lf ./wasm-build0.sh                         # only after src/runtime changes
micromamba run -n xeus-lfortran-wasm-build ./wasm-build1.sh
micromamba run -n xeus-lite-host jupyter lite build \
    --XeusAddon.prefix=$PREFIX --output-dir dist
python3 -m http.server -d dist 8000                           # http://localhost:8000/lab/index.html
```

The rest of this section explains each step.

### 0. Environments (once)

Four environments are involved:

| Environment | Purpose |
| --- | --- |
| `lf` | native dev tools (python, cmake, re2c, bison) for `build0.sh` / `wasm-build0.sh` |
| `xeus-lfortran-wasm-build` | Emscripten toolchain (`emcmake`, `emmake`, `node`) |
| `xeus-lfortran-wasm-host` | WASM *target* libraries: `llvm`, `xeus`, `xeus-lite`, plus the runtime `.mod` files |
| `xeus-lite-host` | `jupyterlite-xeus` + `jupyter_server`, to assemble and serve the site |

```bash
export MAMBA_ROOT_PREFIX=$HOME/micromamba

# Native dev environment, named "lf" (CI uses ci/environment_linux.yml on Linux)
micromamba create -f ci/environment.yml -y
micromamba install -n lf bison=3.4 -c conda-forge -y   # not in the yml on macOS

# Emscripten build toolchain
micromamba create -f environment-wasm-build.yml -y

# WASM host (target) environment
micromamba create -f environment-wasm-host.yml -y \
    --platform=emscripten-wasm32 \
    -c https://prefix.dev/emscripten-forge-4x \
    -c https://prefix.dev/conda-forge

# JupyterLite site builder
micromamba create -n xeus-lite-host jupyter_server jupyterlite-xeus -c conda-forge -y
```

Both build scripts read `PREFIX`, which must point at the *host* environment:

```bash
export PREFIX=$MAMBA_ROOT_PREFIX/envs/xeus-lfortran-wasm-host
```

### 1. Generate the parser/ASR headers

```bash
micromamba run -n lf ./build0.sh
```

### 2. Build the runtime `.mod` files (`wasm-build0.sh`)

```bash
micromamba run -n lf ./wasm-build0.sh
```

This builds a throwaway native `lfortran` (in `asset_dir/`, without LLVM),
compiles `src/runtime/**/*.f90` with it, and copies the resulting `.mod` files
into `$PREFIX/lib/`. They are preloaded into the WASM virtual filesystem at
`/lib/` at link time, which is how `use iso_c_binding` and `use lfortran_display`
resolve in the browser. Verify:

```bash
ls $PREFIX/lib/*.mod
```

Re-run this step whenever you change anything under `src/runtime/`.

### 3. Cross-compile the kernel (`wasm-build1.sh`)

```bash
micromamba run -n xeus-lfortran-wasm-build ./wasm-build1.sh
```

This configures `build-wasm/` via `emcmake` with `-DXEUS_LFORTRAN_WASM_BUILD=yes`
and installs `xlfortran.js`, `xlfortran.wasm` and `xlfortran.data` into
`$PREFIX/bin/`, alongside the kernelspec in `$PREFIX/share/jupyter/kernels/fortran/`
(generated from `share/jupyter/kernels/fortran/wasm_kernel.json.in`).

This is the slow step (it links LLVM); subsequent incremental rebuilds after a
source edit are much faster.

### 4. Run the WASM test-suite

```bash
(cd build-wasm/src/lfortran/tests && \
    micromamba run -n xeus-lfortran-wasm-build node test_lfortran.js)
```

It must be run from that directory, so that the preloaded `.data` file next to
`test_lfortran.js` is found.

This is the same command CI runs, and is the cheapest way to check a WASM-only
bug — no browser involved. See "Writing a test" below.

### 5. Assemble the JupyterLite site

```bash
micromamba run -n xeus-lite-host jupyter lite build \
    --XeusAddon.prefix=$PREFIX \
    --contents share/lfortran/nb/Demo1.ipynb \
    --contents share/lfortran/nb/Demo2.ipynb \
    --contents share/lfortran/nb/Variables.ipynb \
    --contents share/lfortran/nb/Mandelbrot.ipynb \
    --contents share/lfortran/nb/Rich_Display.ipynb \
    --output-dir dist
```

Add `--contents my_bug.ipynb` to ship your own reproducer notebook into the
site.

### 6. Serve it

```bash
python3 -m http.server -d dist 8000
```

or, equivalently,

```bash
micromamba run -n xeus-lite-host jupyter lite serve --output-dir dist
```

Then open <http://localhost:8000/lab/index.html> — the local equivalent of the
deployed URL. A plain `file://` open does **not** work: the kernel is loaded by
a web worker and needs a real HTTP origin.

### Edit → rebuild loop

After changing LFortran sources:

```bash
micromamba run -n xeus-lfortran-wasm-build ./wasm-build1.sh   # (add wasm-build0.sh first if src/runtime changed)
micromamba run -n xeus-lite-host jupyter lite build --XeusAddon.prefix=$PREFIX --output-dir dist
python3 -m http.server -d dist 8000
```

Then hard-reload the browser page (JupyterLite caches aggressively in a service
worker and in browser storage). If stale behaviour persists, open the site in a
private window, or clear site data for `localhost:8000`. Notebook *contents* in
JupyterLite are stored in browser local storage, so an edited notebook keeps its
locally-stored version even after a rebuild — use "File → Reset" or clear
storage to pick up a new `--contents` version.

### Troubleshooting

* `Running build0.sh failed` — `build0.sh` invokes `python` (not `python3`); run
  it inside the `lf` environment.
* `no runtime .mod files found in .../lib` — step 2 was skipped or `PREFIX`
  points at the wrong environment.
* `XEUS_LFORTRAN_WASM_BUILD requires Emscripten` — `wasm-build1.sh` was not run
  through `emcmake`, i.e. not inside the `xeus-lfortran-wasm-build` environment.
* `Module 'x' modfile was not found` in the browser but not natively — the
  module's `.mod` file was not preloaded; check `$PREFIX/lib/*.mod` and relink.
* Blank page or "kernel failed to start" — check the browser console; usually a
  missing `xlfortran.data`, meaning `jupyter lite build` ran against a stale
  `--XeusAddon.prefix`.

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

Inside WASM (assuming the Option B environments exist), the fast incremental
loop is to rebuild just the test binary:

```bash
export PREFIX=$MAMBA_ROOT_PREFIX/envs/xeus-lfortran-wasm-host
micromamba run -n xeus-lfortran-wasm-build emmake make -C build-wasm test_lfortran -j8
(cd build-wasm/src/lfortran/tests && \
    micromamba run -n xeus-lfortran-wasm-build node test_lfortran.js -tc="FortranEvaluator module*")
```

A full `./wasm-build1.sh` (which also relinks `xlfortran`) is only needed before
rebuilding the site.

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
