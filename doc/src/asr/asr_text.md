# ASR text format

ASR text is a lossless representation of an ASR translation unit. It is
intended for compiler tests, generated-input reduction, and direct
experimentation with ASR without going through a language frontend.

The format is an EDN data subset, defined by the grammar below. A standard EDN
reader can read it once handlers for the namespaced `#asr/*` tags are
registered; `tests/asr/check_edn.py` enforces that on every build by reading
printed ASR back with a third-party EDN implementation. LFortran reads the
format as data only; it does not evaluate Clojure code.

A document is a single ASR constructor, normally a `TranslationUnit`. There is
no envelope and no version field: the format is defined by this document, and
a future incompatible encoding would announce itself, with its absence meaning
the encoding described here.

## Printing ASR text

The named form is the default for interactive dumps:

```console
lfortran input.f90 --show-asr --clojure
```

It prints every non-location field using the exact field names in
`src/libasr/ASR.asdl`:

```clojure
(Var
  :v (SymbolRef 1 "x")
)
```

The positional form omits field names but preserves ASDL declaration order.
`asr_clojure` reference tests use this form so the stored dumps stay small:

```console
lfortran input.f90 --show-asr --clojure --no-member-names
```

```clojure
(Var (SymbolRef 1 "x"))
```

`--no-indent` emits either form on one line. Printed ASR text never contains
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
keyword/member pairs. Hand-written `.asr` fixtures may use named form for
readability. `asr_clojure` reference output is positional.

## Grammar

The reader accepts exactly the following. It is a subset of EDN: everything
here reads identically under a conforming EDN reader, but the reverse does not
hold, since EDN forms with no meaning in ASR text (sets, characters, ratios,
namespaced keywords, metadata) are rejected.

```ebnf
document    = value ;

value       = list | vector | map | tagged
            | keyword | symbol | string | integer | float
            | "nil" | "true" | "false" ;

list        = "(" , { value } , ")" ;          (* constructors and products *)
vector      = "[" , { value } , "]" ;          (* ASDL sequences *)
map         = "{" , { value , value } , "}" ;  (* symbol table entries *)
tagged      = "#" , tag , value ;
tag         = "asr/bytes" | "asr/float64" | "asr/real128" | "asr/loc" ;

keyword     = ":" , name ;                     (* member names, enum values *)
symbol      = name ;                           (* constructor names *)
name        = ( letter | "_" ) , { letter | digit | "_" } ;

string      = '"' , { char | escape } , '"' ;
escape      = "\\t" | "\\r" | "\\n" | '\\"' | "\\\\" | "\\u" , 4 * hex ;

integer     = [ "-" ] , digit , { digit } ;
float       = [ "-" ] , digit , { digit } , [ "." , { digit } ]
            , [ ( "e" | "E" ) , [ "+" | "-" ] , digit , { digit } ] ;

hex         = digit | "a".."f" | "A".."F" ;
```

Whitespace, commas and `;` line comments separate values and are otherwise
insignificant. A constructor is a list whose first element is a symbol naming
an ASR constructor; its remaining elements are either all positional values in
`ASR.asdl` declaration order, or `:member value` pairs. The two forms may
appear in the same document but not within one constructor.

## Encoding and text semantics

- **A document is UTF-8.** This is the only encoding the format is defined in,
  and it is what makes the EDN claim meaningful.
- **Fortran character values are byte arrays, not text.** A character constant
  can hold bytes that are not valid UTF-8, such as `achar(200)`. Those have no
  string spelling, so they are written as `#asr/bytes` instead. Writing them
  raw would produce a document no reader can decode, and escaping them as
  `\u00XX` would silently turn one byte into a two-byte character. Wherever a
  string may appear, `#asr/bytes` is therefore also accepted.
- **Only portable escapes are emitted.** `\t`, `\r`, `\n`, `\"`, `\\` and
  `\uNNNN`. Other control characters, including backspace and form feed, are
  written as `\uNNNN` rather than `\b` and `\f`, which are not part of EDN.
- **Round-tripping is byte-exact.** Printing ASR, reading it back and printing
  it again yields identical bytes, including for values that take the
  `#asr/bytes` path.
- **Floating point is exact.** Finite values print with enough digits to
  round-trip; infinities and NaNs print as `#asr/float64` bit patterns, since
  EDN has no literal for them. `real(16)` uses `#asr/real128`.

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
