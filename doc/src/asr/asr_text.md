# ASR text format

ASR text is a lossless, versioned representation of an ASR translation unit.
It is intended for compiler tests, generated-input reduction, and direct
experimentation with ASR without going through a language frontend.

The format is an EDN-compatible data subset. A standard EDN reader can read it
when handlers for the namespaced `#asr/*` tags are registered. LFortran reads
the format as data only; it does not evaluate Clojure code.

Every document is wrapped in `ASRText`. Its `version` field selects the text
encoding version independently of the LFortran or `ASR.asdl` version, and its
`value` field contains the translation unit.

## Printing ASR text

The named form is canonical:

```console
lfortran input.f90 --show-asr --clojure
```

It prints every non-location field using the exact field names in
`src/libasr/ASR.asdl`:

```clojure
(ASRText
  :version 1
  :value (Var
    :v (SymbolRef 1 "x")
  )
)
```

The positional form omits field names but preserves ASDL declaration order:

```console
lfortran input.f90 --show-asr --clojure --no-member-names
```

```clojure
(ASRText 1 (Var (SymbolRef 1 "x")))
```

`--no-indent` emits either form on one line. Canonical ASR text never contains
color escape sequences, elides intrinsic modules, or truncates constant data.

The legacy output of `--show-asr` without `--clojure` remains available during
the format migration.

## Reading ASR text

Files ending in `.asr` are read as ASR text:

```console
lfortran program.asr
```

`--from-asr` selects ASR text explicitly when a different extension is used.
The first implementation supports one ASR translation unit with the default
LLVM backend, including `--show-asr`, `--show-llvm`, `--pass`, `--skip-pass`,
`-S`, `-c`, and executable linking. When linking an executable, the ASR
translation unit itself must contain exactly one main program; additional
object files and libraries provide dependencies rather than the entry point.

The parser accepts named and positional constructors. The two forms can appear
in one file, but a single constructor cannot mix positional members with
keyword/member pairs. Committed regression fixtures should use named form.

`--verify-asr` parses a standalone ASR file and runs only its initial verifier.
It does not execute ASR passes or code generation, so corpus runners can
distinguish an accepted initial verifier rejection from any later compiler
failure.

## Data forms

ASR text uses:

- lists for constructors and ASDL products;
- keywords for member names and enum values;
- vectors for sequences;
- maps for symbol table entries;
- strings for identifiers and string values;
- `nil` for absent optional fields;
- `true` and `false` for logical fields;
- EDN integers and finite floating-point values;
- semicolon comments and optional commas.

All non-location fields are explicit in canonical named output, including
absent optional fields, empty sequences, and false logical values.

`SymbolRef` is a reserved text-format form that identifies a symbol by its
document-local symbol table ID and exact symbol-table key:

```clojure
(SymbolRef 1 "x")
```

Other ASR-specific values use namespaced tagged elements:

- `#asr/bytes "..."` stores an exact raw constant payload;
- `#asr/float64 "..."` and `#asr/real128 "..."` preserve values that cannot
  round-trip through an EDN decimal;
- `#asr/loc [[first last] value]` overrides a generated text location with
  inclusive byte offsets in focused tests.

The compact spelling `@x` is not used because a Clojure reader interprets `@`
as the dereference reader macro.

## Graph references

ASR contains a graph rather than a pure tree. Symbol tables own symbol
definitions, while expressions, types, and other symbols refer back to those
definitions. Canonical text assigns deterministic integer IDs such as `0` and
`1` to owning symbol tables before printing any references.

The decoder first creates all symbol tables and typed symbol shells, then fills
definitions and resolves references. This permits forward references and
cycles while preserving pointer identity.

## Locations and diagnostics

Original Fortran byte offsets are not included by default. Each parsed ASR node
is assigned the location of its constructor token in the `.asr` file. ASR
parser and verifier diagnostics therefore highlight the direct ASR input:

```text
ASR verify pass error: ...
 --> example.asr:12:4
```

Explicit location tags are reserved for tests that need a specific span.

## Parser and verifier boundary

The text parser rejects malformed EDN, unknown constructors or fields, missing
required fields, wrong field categories, duplicate definitions, and unresolved
textual references.

It does not enforce semantic ASR invariants. Once decoding succeeds,
`asr_verify` is responsible for type, rank, symbol, ownership, and other ASR
requirements. Consequently:

- malformed text produces an ASR syntax diagnostic;
- structurally decoded but invalid ASR produces an ASR verifier diagnostic;
- verifier-valid standalone ASR proceeds through the normal pass and LLVM
  pipeline.

## Regression corpus

`tests/asr/tests.toml` registers direct ASR regression fixtures with one of two
expected outcomes:

- `compile`: initial verification succeeds and the production compiler emits a
  nonempty executable;
- `verify`: initial verification fails with the exact stable diagnostic code,
  message, and constructor-token span declared by the manifest.

The registered runner first calls `--verify-asr`. It only invokes the normal
LLVM and linker pipeline for fixtures whose expected initial result is
successful verification, with `--verify-all-passes` enabled independently of
the compiler build type. This prevents a compile regression from being hidden
by making the initial verifier reject previously valid ASR and identifies a
pass that corrupts previously valid ASR as a post-initial compiler failure.

## Deterministic mutation fuzzing

`tests/asr/fuzz.py` dynamically prints the verified pre-pass ASR produced from
registered integration-test seeds, applies one field-aware mutation, and runs
the two-outcome oracle in isolated compiler subprocesses.

```console
python tests/asr/fuzz.py \
  --lfortran src/bin/lfortran \
  --seed 1234 \
  --cases 1000
```

Each case first runs initial verification. A verifier rejection is accepted;
verifier-valid ASR must then emit both an object and an executable with
post-pass verification enabled. Timeouts, signals, parser failures, pass
verification failures, LLVM failures, object failures, and link failures are
persisted under `asr-fuzz-artifacts/` as the exact ASR input plus JSON metadata.
The metadata records the source integration test, random seed, case index,
mutation, input hashes, failing phase, commands, and output.

Persisted failures can be replayed without regenerating or mutating the seed:

```console
python tests/asr/fuzz.py \
  --lfortran src/bin/lfortran \
  --replay asr-fuzz-artifacts/failure-000000-....json
```

The structural reducer greedily removes vector elements and symbol-table
entries, replaces optional fields with `nil`, and shrinks numeric values while
requiring the same initial-verification status and normalized failure phase:

```console
python tests/asr/reduce.py \
  --lfortran src/bin/lfortran \
  --metadata asr-fuzz-artifacts/failure-000000-....json
```

It writes a canonical `.min.asr` file and reduction journal without modifying
the original failure artifact.
