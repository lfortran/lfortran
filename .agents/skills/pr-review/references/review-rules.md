# Review Rules

## 1. Make ASR explain the program

The backend should translate explicit ASR, not discover semantic facts from
LLVM types and repair them with casts. For each backend branch, ask whether:

- AST-to-ASR can resolve the decision;
- an ASR field, enum, or structured type can record it;
- ASR verification can enforce the resulting invariant; or
- an ASR pass can canonicalize the representation.

Type coercion, semantic casts, overload selection, and source-language
decisions belong in AST-to-ASR. Backend type inequality is not a semantic
predicate.

## 2. Prefer explicit state and existing abstractions

Do not infer intent from strings, argument positions, or backend artifacts.
Pass explicit options from the caller that owns the decision. Normalize related
types once and share symbol lookup, parent walking, bounds, index creation,
element references, and loop construction.

Identify intrinsic types, modules, and special compiler entities through their
structured origin or explicit ASR state. A user declaration can reuse a familiar
name, so comparing only `m_name` or module-name strings is not a sound semantic
test.

Distinguish user-visible from compiler-synthesized instances of the same node or
symbol kind through explicit provenance. Derive that state from the canonical
definition and owner, then store it in ASR when consumers need it. Do not encode
internal facts in reserved or mangled name prefixes and scan those strings
later.

Keep utilities at the narrowest reusable layer:

- use `ASRUtils` for frontend-independent ASR operations;
- use `PassUtils` for pass-specific construction and lowering;
- use existing codegen helpers for target-specific mechanics;
- do not expand a core API solely to serve one local lowering.

## 3. Keep errors explicit

New exceptions move error state outside the type system and force catch-based
control flow. Propagate expected compiler failures through `Result` or the
established explicit status type. Attach a real source `Location` to
diagnostics and preserve it through intermediate processing.

Do not add broad catches, silent fallbacks, or success-shaped error paths.

For states that cannot legitimately occur in valid ASR, prefer an assertion or
verifier requirement over a defensive conditional. A conditional that silently
continues can hide the phase that created invalid state.

An assertion must test the actual boolean invariant; a descriptive string is a
truthy value and never fires. Do not skip a formal/actual count mismatch or
similar impossible state with `continue`. Assert it locally, fix the producing
phase, and generalize the invariant in ASR verification when it applies
globally.

## 4. Accept extensions honestly

When LFortran deliberately accepts useful input that the standard forbids, do
not accept it silently and do not reject it merely because it is an extension.
Emit a located portability warning that:

- names the non-conforming construct or constraint;
- identifies the behavior as an LFortran extension; and
- suggests an actionable standard-conforming alternative.

Test the warning text and source label in a reference diagnostic. Test runtime
behavior separately, using only compiler labels that accept the extension and a
short comment explaining any omitted reference compiler.

Accepting a deliberate extension differs from pretending an unimplemented
standard construct works. Do not lower an unsupported construct to a convenient
existing node that executes only its body while discarding its semantics.
Reject it clearly, or represent the unsupported state explicitly and emit a
located warning that behavior is being ignored.

## 5. Test semantics and the actual path

Every test must execute the exact binary, backend, options, and code path
changed by the fix. A nearby target or similar configuration is not evidence.

For each new semantic branch or error:

- add a compact MRE-derived regression;
- prove that it fails without the fix and passes with it;
- include the edge case that could disprove the implementation;
- cover scalar and array forms, sections, ranks, inheritance, type-bound
  procedures, nondefault kinds, and target differences when relevant;
- register end-to-end tests and use both `gfortran` and `llvm` labels where
  supported.

Keep fixtures proportional to the bug. A large unrelated source file is not an
acceptable substitute for a minimal regression.

Put recoverable semantic errors in the appropriate continue-compilation file.
Use a complete program that could otherwise build, include multiple independent
errors to prove recovery, and temporarily disable each error in turn to verify
that every diagnostic branch is genuinely exercised.

Keep shared error-recovery fixtures append-only: place reusable helper
procedures in a module at the top, then append new independent cases at the end.
This prevents unrelated diagnostic source locations and reference output from
shifting on every addition.

Differential C interoperability tests must include the toolchain-provided
`ISO_Fortran_binding.h` and link the matching runtime. Forcing LFortran's private
header or allocator shims into a GFortran or Flang run invalidates the
comparison.

When the test program's own standard legality is uncertain, confirm it against
an independent language reference or a second compiler before encoding the
behavior as supported. Prefer an established test declaration idiom over
inventing a new one.

Build, generation, and validation steps that can fail must execute on pull
requests. Exclude only operations inherently restricted to the main branch,
such as publishing with main-only credentials.

## 6. Check kinds, layout, targets, and runtime behavior

Do not assume:

- the default index kind when `descriptor_index_64` can change it;
- host ABI sizes for C interoperable kinds;
- compile-time knowledge when a value can be dynamic;
- contiguous memory implies equivalent language semantics;
- scalar and array physical representations are interchangeable.

Trace kinds, ranks, ownership, descriptor metadata, record boundaries, and ABI
facts from ASR through the changed code.

In runtime C, distinguish a buffer's capacity from the desired formatted
length. Pass the actual capacity to `snprintf`-style APIs, preserve room for
terminators according to the allocation contract, and avoid out-of-range
floating-to-integer conversions or other undefined behavior. A
platform-specific workaround needs a tracked defect, a minimal reproducer, and
a focused runtime test.

When a field has physically different representations selected by a kind or
other discriminant, route every read through one discriminant-aware accessor or
use distinct explicit fields. A reader that ignores the discriminant silently
misinterprets the bits. Do not add a field that duplicates information already
available from the type. Do not use floating-point approximations to detect
integer overflow; use exact or bounded integer reasoning.

Whether a derived type has a defined storage layout is a semantic fact. Record
properties such as `SEQUENCE` and `BIND(C)` explicitly in ASR, use them to drive
member representation, and verify them before code generation. Raw byte
operations such as `c_loc`, `storage_size`, `transfer`, and `memcpy` require
special scrutiny when an aggregate otherwise uses descriptor-based or
compiler-defined layout.

## 7. Preserve symbol-reference invariants

Keep symbol relationships canonical:

- `ExternalSymbol::m_external` points to the original definition, never another
  `ExternalSymbol`; resolve through `symbol_get_past_external` before importing.
- Symbol creation is idempotent; check the current scope and reuse a compatible
  existing symbol instead of adding a duplicate.
- Parent and owner links identify the actual defining scope.
- Only symbol nodes own `SymbolTable`. When a statement introduces local names,
  place a `Block` or dedicated symbol node in its body and verify that the
  required scope-bearing node is present.
- Replacing an existing symbol-table entry is unsafe because resolved references
  can still point to the old entry. Skip an equivalent existing definition or
  assert identity instead of overwriting it.

Enforce these invariants when importing or constructing symbols. Repairing
chains or duplicates later spreads defensive logic across passes and backends
and leaves ASR in an ambiguous state.

## 8. Choose the earliest correct layer

Move a decision earlier only when the invariant is true for every input handled
at that layer. Centralizing logic in a deep shared function is wrong if that
function also handles inputs with different semantics.

For example, file-level normalization belongs at file-like entry points when a
shared preprocessor function also processes inline macro fragments. The narrow
entry layer is earlier and safer than changing the deepest shared routine.

Syntactic distinctions belong in the grammar and AST. Route them through
AST-to-ASR into the same canonical semantic form as equivalent syntax rather
than adding a backend special case.

Parser and grammar actions must still construct AST for semantically invalid
input. Perform duplicate-name checks, type restrictions, and other semantic
validation in AST-to-ASR, where diagnostics have precise entity locations and
continue-compilation recovery is available.

Treat lexer and grammar as one interface. Every newly emitted token needs a
matching grammar production and parser-level test in the same change; audit all
sibling lexer paths that emit it. When a construct must compose in several list
contexts, extend the singular per-item production so all existing list wrappers
inherit it rather than patching every concatenation rule.

## 9. Add ASR passes selectively

A new ASR pass is appropriate when it:

- performs a coherent non-local transformation;
- gives several consumers one canonical representation;
- replaces duplicated complex algorithms;
- establishes a clear, verifiable output invariant; and
- has understandable ordering and can be tested independently.

Prefer an ASR field plus verifier, an existing pass, or direct AST-to-ASR
construction for local decisions. Every new pass adds compile time, ordering
dependencies, maintenance, and another failure boundary.

A new ASR node is not reviewable until exhaustive consumers have a minimal,
correct case for it: verifier, dependency walkers, serializers, and round-trip
printers. A reachable "not implemented" path in one of these consumers is a
blocker. Prefer inspecting a new transformation through Fortran regenerated
from its output ASR; reserve raw serialized-ASR assertions for narrow details.

Capability signals are user contracts. Do not change a supported-kind check,
sentinel, or intrinsic-module constant to advertise a feature until it works
end-to-end on every claimed target. Keep partial support marked unsupported or
place it behind an explicit opt-in.

## 10. Keep backend changes mechanical

Some target-specific work cannot move earlier. In that case:

- keep the branch small and local;
- reuse `asr_to_llvm`, `llvm_utils`, `ASRUtils`, and existing helpers;
- avoid duplicated loop builders and long one-off blobs;
- do not hard-code argument positions or kinds;
- make target conditions explicit; and
- require ASR to provide every semantic decision the backend needs.

Hide toolchain-version differences behind one version-independent helper with
the conditional isolated inside it. Repeating the same version guard at several
call sites is a rework finding. Explain long pointer, descriptor, and stride
logic in semantic terms, then shrink it to a few calls or decompose it into
named helpers that reuse existing utilities.

## 11. Keep changes narrow and reviewable

Separate behavioral changes from formatting, generated-output churn, broad
refactoring, and unrelated cleanup. Avoid shifting a large reference output for
one small diagnostic. Make preparatory mechanical changes separately when they
would otherwise obscure the behavior under review.

Be especially careful when weakening safety or CI checks. Add a negative test
that proves the check still rejects its target.

For external-project failures, first identify which compiler, runtime, or
dependency fails. Do not import an upstream disable patch or skip broad coverage
when the failure belongs to a different toolchain. If quarantine is unavoidable,
make it narrow, link it to a tracked defect, and preserve all unaffected
coverage.

Distinguish gratuitous reference churn from necessary serialization changes.
Adding an ASR field can legitimately regenerate many outputs; review those
changes for mechanical consistency and keep unrelated behavioral changes out of
the same regeneration.

Build-system changes must track the files they consume through dependency lists
or reconfigure-on-change mechanisms, quote paths that can contain spaces, and
fail during configuration when a required generated artifact cannot exist.
Do not commit binary blobs or large generated payloads when a small textual
fixture or generated test can exercise the same behavior.

Comments must describe implemented behavior. A comment claiming a dispatch,
delegation, or fallback that does not exist is a defect; recheck comments when a
pass is renamed or restructured. Regenerate toolchain-sensitive reference output
with the project's pinned or minimum supported toolchain rather than an
arbitrary local version. Remove dead code introduced by the change, and move
incidental discoveries into separate tracked work instead of expanding scope.

## 12. Distinguish blockers from follow-ups

An acceptable incremental change can still leave bounded debt. Classify it
explicitly:

- **Blocker:** wrong semantics, invalid ASR, regression, unsafe error handling,
  or untested core behavior.
- **Rework:** placement or maintainability problem that shapes the current
  implementation and should be fixed before merging.
- **Follow-up:** bounded debt that does not invalidate the current change.

State the exact test, invariant, or refactor required by a follow-up. Do not let
approval of the overall direction hide a known correctness issue.

Treat findings from static analyzers and automated review tools as hypotheses.
Confirm the failure with an MRE, understand the causal path, and then report the
verified defect in the reviewer's own technical explanation.
