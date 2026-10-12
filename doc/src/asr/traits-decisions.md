# Traits proposal: decisions, ambiguities and open questions

This document records the questions that came up while implementing the traits
proposal in LFortran: what was asked, which options were presented, what was
chosen, and where the proposal leaves room for more than one reading. Its
purpose is to improve the proposal, so that independent compiler vendors can
implement it without having to ask the same questions again.

It is a living document. Last updated: 2026-10-12.

## About this record

- The recorded choices are working agreements for the LFortran prototype. They
  are not part of any Fortran standard, and they are not a ratification of the
  proposal by its author or by a standards committee.
- A "not implemented yet" diagnostic of the prototype is an implementation
  limit, not a language rule. Unless an entry says otherwise, a program that
  the prototype rejects only for such a reason should be accepted by a complete
  implementation.
- This is an initial inventory, assembled from the implementation's record of
  questions and from the acceptance records of the paper examples. It is not
  claimed to be a complete history. Add new questions as they arise (see
  "Adding an entry" at the end).
- What the compiler currently does is described in [Experimental
  traits](traits.md), and for `:=` in [usage](../usage.md). This document
  records why, and what the proposal should say.

## Sources and conventions

- **Paper.** `traits.tex` of
  [difference-scheme/Traits-for-Fortran](https://github.com/difference-scheme/Traits-for-Fortran)
  at commit
  [`5163a026`](https://github.com/difference-scheme/Traits-for-Fortran/tree/5163a026967c165d77b033aaa1476c96feba53bd)
  (SHA-256 of `traits.tex`:
  `fea5d29ce2cd58314fb282d86098ebcbcabf605842397d63349c4554cad6ca21`). Line
  numbers, section labels such as `sect:tats`, and listing paths such as
  `Code/Fortran/vector.f90` refer to that commit.
- **Paper programs.** The eight complete programs `functional1`, `functional2`,
  `mixed`, `static`, `printy`, `ocp`, `overloading` and `vector`, and the two
  module-only examples `Interop/extends_parent` and `Interop/abstract_new`, all
  under `Code/Fortran/`.
- **Inline listings.** `inline-NN` is the NN-th Fortran listing written
  directly in the text of the paper, counted in paper order from chapter
  `chapt:traits` on. The 35 core listings, inline-01 to inline-35, are in the
  two Fortran-extension chapters (lines 1031-2393). `inline-36` (lines
  2889-2898) is the schematic union-type example of the Outlook chapter and is
  out of scope.
- **Public discussion.** Issue
  [difference-scheme/Traits-for-Fortran#2](https://github.com/difference-scheme/Traits-for-Fortran/issues/2),
  "Clarify trait implementation visibility and separate compilation", called
  *the visibility issue* below, and pull request
  [difference-scheme/Traits-for-Fortran#3](https://github.com/difference-scheme/Traits-for-Fortran/pull/3),
  "Add trait examples and a restrictive conformance plan", called *the
  restrictive-plan PR* below. Both are in the proposal repository, not in
  LFortran's.
- **People.** The *maintainer* is the LFortran developer who directed this
  prototype and answered the questions below. The *proposal author* is the
  author of `traits.tex`. The *implementers* prepared the questions and the
  code.
- **Dates** are the UTC dates on which a question was asked.

## Status legend

| Status | Meaning |
| --- | --- |
| Approved semantic decision | The maintainer chose a language-level rule for the prototype. It is candidate text for the proposal, not an accepted change of it. |
| Approved prototype option | The maintainer chose a scope, a priority, a validation policy or an implementation strategy. It implies no language rule. Temporary limits belong here. |
| Implementation choice | Made by the implementers without a dedicated question to the maintainer, and reviewed only as code. Revisit it before relying on it. |
| Erratum | A clerical correction of a paper listing, applied only to an acceptance copy. |
| Open question | Not decided. The proposal needs to answer it. |
| Paper explicit | The proposal already answers the question. Recorded so that it is not asked again. |

An entry can have more than one status, for example an approved temporary limit
together with the open language question behind it.

## At a glance

Where the prototype deliberately departs from, narrows or reinterprets the
paper:

- A conformance declared outside the type's own module is visible only where
  the module declaring it, or a facade re-exporting it, is used. Importing the
  type from its own module, or an intrinsic kind constant, is not enough
  (TD-07).
- A receiver may be modified (`intent(inout)`) only by direct calls on concrete
  objects; generic trait calls and trait objects are read-only (TD-10).
- Initializer requirements are checked as conformance obligations but are not
  dispatched through trait objects (TD-13). Besides `initial ::` procedures, a
  matching specific of a standard constructor interface named like the type
  can implement them (TD-33).
- Five listings are corrected in acceptance copies (four `END TYPE` errata and
  the SAXPY result), and missing context is completed (TD-29 to TD-31).
- Several temporary limits are diagnosed rather than implemented (TD-24 to
  TD-27). They are not proposed language rules.

The most important open questions for the proposal are collected in "Open
questions for the proposal".

## Questions put to the maintainer

The table lists, in order, every question about the language, the paper or the
scope of the prototype that the implementers put to the maintainer as an
explicit choice between options, as recorded in the implementation's question
log. The chosen option is in **bold**. Questions about CI, branches and review
gating are listed separately in "Questions that do not affect the language".

| Date | Question | Options presented | Entry |
| --- | --- | --- | --- |
| 2026-10-04 | Implementation scope | **Full proposal roadmap, with a small end-to-end first milestone**; the entire proposal in one effort; only a minimal proof of concept | TD-01 |
| 2026-10-04 | Missing companion example sources | Plan from the available text and obtain the sources early; the maintainer provides them. Answered in free text: the files are in a clone of the proposal repository, whose paper is older; follow the supplied paper. | TD-04 |
| 2026-10-04 | Initial feature rollout | Opt-in experimental extension; **enable the new syntax by default as an additive extension** | TD-03 |
| 2026-10-04 | Visibility of retroactive implementations | **Start with explicitly imported implementation modules; keep the paper's broad visibility as an unresolved design milestone**; require the broad visibility from the start; leave both open | TD-07 |
| 2026-10-04 | Implementation boundary | **Share the existing generic machinery incrementally; add an explicit trait/conformance representation**; keep traits separate initially and investigate unification later | TD-02 |
| 2026-10-04 | First executable milestone | **One nominal trait, `implements` and constrained generic calls for two derived types**; a numeric type-set trait with a generic array sum | TD-01 |
| 2026-10-04 | Unsettled language details (associated-type spelling, `:=` loop scoping, runtime dispatch of generic methods) | **Record them as design gates before the affected milestones**; resolve them before finalizing the plan | TD-01 |
| 2026-10-04 | Identical signatures inherited from different parent traits | **Coalesce the callable requirement, retaining every nominal obligation**; reject as ambiguous for now | TD-05 |
| 2026-10-04 | Parent conformance available directly and through a child | **Accept when the canonical procedures and receiver bindings agree; reject conflicting witnesses**; require one canonical conformance record even when they agree | TD-05 |
| 2026-10-05 | Priority of kind wildcards, given inconsistent `real(10)` support | Defer full `real(*)`; fix `real(10)` first; defer all wildcards. Answered in free text: settle runtime polymorphism first, then smaller items such as `real(*)`. | TD-01, TD-17 |
| 2026-10-05 | Receiver intent contract | **Read-only default; declare mutation intent explicitly per method**; keep the prototype read-only and defer the design; mutable default | TD-10 |
| 2026-10-05 | Concrete type inspection through trait objects | **Permit ordinary concrete `SELECT TYPE` inspection**; keep trait payloads opaque | TD-11 |
| 2026-10-07 | Lifetime of repeated factory results in one expression | **Implement general temporary retention now**; diagnose such contexts and implement retention next | TD-14 |
| 2026-10-07 | Scope of anonymous `class(A + B)` combinations | **Implement general combinations now, including conversions declared by later clients**; a bounded slice first | TD-12 |
| 2026-10-08 | Authoritative example sources | **Existing companion files; root `traits.tex` authoritative; discrepancies flagged**; a newer revision to be provided | TD-04 |
| 2026-10-08 | Examples that must pass | **All concrete Fortran examples and inline listings of the core proposal; Outlook separate**; also the Outlook example | TD-04 |
| 2026-10-08 | Correct the type-closing statements of inline-05, -07 and -09 in acceptance copies | **Yes** (yes/no) | TD-29 |
| 2026-10-09 | Scope of automatic discovery of augmentations for `printy.f90` | Current compilation unit first; also cross-file discovery. Answered in free text with a request for a detailed example, followed by the exchange in TD-07. | TD-07 |
| 2026-10-09 | Repository for the visibility issue | **difference-scheme/Traits-for-Fortran**; lfortran/lfortran | TD-07 |
| 2026-10-09 | SAXPY result in inline-01 | **`real :: res(size(x))`**; `real, allocatable :: res(:)`; keep `res(:)` and design a rank-only rule | TD-30 |
| 2026-10-10 | Runtime generic methods over the closed numeric type set | **Provider-owned entries for each member now; open-world arrays and results for OCP later**; the general open-world ABI first | TD-15 |
| 2026-10-10 | Parameter list of a generic type's `implements` statement | **Exact repetition for now**; renamed binders; inferred omitted list | TD-24 |
| 2026-10-10 | Method and initializer type parameters | **No shadowing and no extra initializer parameters for now**; full lexical scopes | TD-24 |
| 2026-10-10 | Implementation procedures that depend on a generic type's parameters | **One owning generic type, reached through its bindings and constructors, for now**; direct calls and sharing | TD-24 |
| 2026-10-11 | Receiver mutation in `vector.f90` | **Allow mutation on direct concrete calls; reject mutating bindings on read-only trait paths**; design explicit mutating-message syntax and edit the example; keep read-only and defer Vector | TD-10 |
| 2026-10-12 | Host association of generic instances | **Diagnose now; full support after the remaining paper examples**; full support now | TD-26 |
| 2026-10-12 | Reallocation mode for `vector.f90` | **Use `--realloc-lhs-arrays` in Vector's acceptance commands, keeping the source unchanged**; implement the behavior without the flag first | TD-28 |
| 2026-10-12 | Unused bindings of generic instances | **Located temporary diagnostic; decide the general policy after the paper examples**; check and instantiate only used bindings; require every binding to be valid | TD-27 |
| 2026-10-12 | Correct the type-closing statement of inline-08 | **Correct it in the acceptance copy only**; leave it as an unresolved paper error | TD-29 |
| 2026-10-12 | Initializer requirements in runtime views | **Check initializer conformance; omit constructors from runtime dispatch slots**; a different runtime-view design | TD-13 |
| 2026-10-12 | Static-only providers outside the runtime subset (`ocp.f90`) | **Preserve static use; diagnose only a requested runtime use**; reject such providers at definition time for now | TD-16 |
| 2026-10-12 | Existing standard constructor interfaces (`interface MyType`) as implementations of initializer requirements | **Allow matching standard constructor interfaces as initializer implementations**; require an explicit `initial ::` binding | TD-33 |

### Instructions given without an option form

| Date | Instruction or exchange | Entry |
| --- | --- | --- |
| 2026-10-04 | Asked for a comparison of LFortran's existing templates with the proposed traits, to decide between reuse/unification and a parallel implementation. | TD-02 |
| 2026-10-05 | Approved doing runtime polymorphism first, in the order runtime contract, ordinary dynamic dispatch, generic-method ABI prototype. | TD-01 |
| 2026-10-08 | Made the paper's own examples the completion criterion: every one of them must work. | TD-04 |
| 2026-10-09 | Rejected implicitly importing other modules when compiling a main program, asked for alternatives closer to existing Fortran (accepting a deviation from the paper), asked to file them as an issue, and chose explicit provider `USE` to unblock `printy.f90`. | TD-07 |
| 2026-10-09 | Every paper example must get a working test; missing boilerplate may be added; design questions must be asked. | TD-04, TD-31 |
| 2026-10-09 | Supplied the updated proposal sources (commit `5163a026`), whose `traits.tex` is identical to the paper used here. | TD-04, TD-32 |

## Entries

Each entry has a stable identifier. Entries are grouped by topic, and
identifiers are assigned in the order in which entries were added, so a later
entry such as TD-33 can appear in an earlier group. The fields are: the
question and why the paper does not settle it, the options presented, the
choice, its rationale and consequences, the implementation status, and the
specification action, that is, what the paper should say.

### A. Scope and validation

#### TD-01: Prototype scope, first milestone and priorities

- **Status:** Approved prototype option.
- **Question.** How much of the proposal should the prototype cover, what
  should be implemented first, and how should language details that the paper
  leaves open be handled?
- **Options and choices.** See the 2026-10-04 and 2026-10-05 rows above. The
  maintainer chose the full roadmap with a small first milestone (one nominal
  trait, `implements`, and constrained generic calls for two unrelated derived
  types), and chose to record open language details as design gates instead of
  blocking that slice. When asked about kind wildcards, the maintainer instead
  asked to settle runtime polymorphism first, and approved the order runtime
  contract, ordinary dynamic dispatch, generic-method ABI prototype. On
  2026-10-08 the completion criterion became the paper's own examples (TD-04).
- **Consequences.** The scope is the two Fortran-extension chapters (lines
  1031-2393) and the worked examples of `chapt:fortran_examples`. The Outlook
  chapter (rank and attribute genericity, union types, structural subtyping,
  lines 2838-2931) is not implemented. The paper's suggestion to retire
  `deferred` bindings eventually (`sect:abstract_types`) is not implemented as
  a change to standard Fortran.
- **Implementation status.** Followed.
- **Specification action.** None.

#### TD-02: Reuse of LFortran's existing generics machinery

- **Status:** Approved prototype option (compiler architecture; no language
  rule).
- **Question.** LFortran already had requirements and templates from an earlier
  generics prototype. The maintainer asked whether traits could reuse or unify
  with them, or needed a parallel implementation.
- **Options presented.** Share the existing generic machinery incrementally and
  add an explicit trait and conformance representation (chosen); keep traits
  separate initially and investigate unification later.
- **Rationale.** Requirements and templates already check generic bodies at
  definition time, substitute types, specialize, import and serialize. Traits
  add what they lack: nominal trait identity, conformance evidence,
  receiver-independent signatures, associated types, type sets, call-site
  inference and trait objects. A second engine would duplicate substitution and
  specialization; translating traits into templates would lose nominal identity
  and the meaning of trait objects.
- **Consequences.** Traits, conformances, constraints and the evidence selected
  for a generic call are explicit compiler state that survives module files.
  Specialization uses the shared instantiator. Conformance stays nominal, with
  no structural fallback. Existing templates keep their behavior.
- **Implementation status.** Followed throughout.
- **Specification action.** None. For vendors: the prototype suggests that the
  proposal can be layered on an existing template engine with definition-time
  checking, provided conformance evidence is made explicit.

#### TD-03: Default-on rollout

- **Status:** Approved prototype option (LFortran policy).
- **Question and options.** An opt-in experimental extension that preserves
  existing Fortran and templates, or the new syntax enabled by default as an
  additive extension (chosen).
- **Consequences.** New words such as `implements`, `sealed`, `initial` and
  `itself` are not reserved and remain usable as ordinary identifiers (for
  example `initial`, also in fixed-form source). Existing programs keep their
  meaning. Trait code receives a portability warning.
- **Implementation status.** Implemented.
- **Specification action.** The paper introduces these words without saying
  whether they are keywords in a reserved sense. Consistent with Fortran, it
  could state that none is reserved, and list the statement forms in which each
  is recognized.

#### TD-04: Authoritative sources and the acceptance corpus

- **Status:** Approved prototype option (validation policy).
- **Questions and choices.**
  - 2026-10-04: the paper includes listings from companion files that were not
    supplied with it. The maintainer pointed to a clone of the proposal
    repository whose paper was older, and said to follow the supplied paper and
    to treat the companion files as possibly older.
  - 2026-10-08: use the existing companion files, with the supplied
    `traits.tex` authoritative and discrepancies flagged; acceptance covers all
    concrete Fortran examples and inline listings of the core proposal, with
    the Outlook separate. The same day the maintainer made the paper examples
    the completion criterion.
  - 2026-10-09: every example must get a working test; missing boilerplate may
    be added; design questions must be asked. The maintainer then supplied
    commit `5163a026`, whose `traits.tex` is identical to the paper used here
    and whose sources resolve the older spellings (TD-32).
- **Consequences.**
  - The corpus is 8 programs, 2 module-only examples and 35 inline listings.
  - Raw originals are preserved, and every adaptation is recorded with its
    reason. An adaptation may complete missing context, but must not remove a
    demonstrated feature or replace it with a simpler construct. A missing
    compiler feature is never a reason to rewrite an example.
  - An executable example must produce its expected output; a zero exit status
    alone is not acceptance. The module-only examples have intentionally empty
    illustrative bodies, so they are verified and compiled to objects but not
    run, and no output is invented for them.
  - Intentionally invalid code must receive a located diagnostic.
- **Implementation status.** At the time of writing, all eight programs,
  including `ocp.f90` (TD-16), and both module-only examples are accepted in
  this branch. Of the 35 core inline listings, recorded in
  `integration_tests/traits_paper_inline/provenance.json`, 33 are accepted:
  `traits_paper_inline.py` checks 31 in normal and fast modes, and
  `traits_paper_manual_values.f90` accepts inline-25 and inline-28.
  inline-17 (TD-17) and inline-20 (TD-18) wait for compiler features.
- **Specification action.** None for the policy. The adaptations in TD-29 to
  TD-32 are candidates for corrections of the paper.

### B. Conformance and visibility

#### TD-05: Composition of traits and overlapping conformances

- **Status:** Approved semantic decision.
- **Why the paper is not tight.** `sect:extends_spec` (lines 1146-1175) defines
  multiple trait inheritance with `extends(A + B)` and says that the child is a
  subtype of each parent. It does not say what happens when two parents declare
  the same message with the same signature, when a conformance can be reached
  in more than one way (directly, through a child conformance, through a
  re-export, or through a diamond), or when parents declare same-named messages
  with different signatures.
- **Options presented (2026-10-04).** Identical inherited signatures: coalesce
  the callable requirement while retaining every nominal obligation (chosen),
  or reject as ambiguous for now. Overlapping conformance paths: accept them
  when the canonical implementation procedures and receiver bindings agree and
  reject conflicting witnesses (chosen), or require a single canonical
  conformance record even when they agree.
- **Approved rules.** Identical requirements from independent parents form one
  callable requirement, but each originating trait remains a separate nominal
  obligation. Overlapping paths are accepted only when they agree on the
  canonical implementation procedures and receiver bindings; otherwise they are
  diagnosed. Import order never selects an implementation.
- **Related rules** (from the paper's subtyping rule or chosen by the
  implementers consistently with it): a requirement inherited through several
  paths from one ancestor (a diamond) counts once; a child conformance implies
  the parent conformances, never the reverse; `implements (A + B) :: T`
  establishes both traits but not a named child trait declared with
  `extends(A + B)`; exact equivalence of two requirements covers dummy names,
  types, kinds, ranks, array shape categories and extents, character lengths
  and procedure and dummy attributes, but not result variable names, so
  assumed-shape and assumed-size dummies differ.
- **Implementation status.** Implemented for ordinary messages, operators and
  initializers. Same-named ordinary messages with different signatures
  inherited from different parents are a separate stage; operator and
  initializer requirements can already be overloaded across a hierarchy. Known
  follow-up: equal acknowledgements of intrinsic operations declared in
  different program units currently get different internal identities and can
  be reported as conflicting, although they denote the same intrinsic operation
  and should always agree.
- **Specification action.** Define requirement identity (by name and signature,
  by originating trait, or both), the signature equivalence relation, the
  agreement rule and diagnostics for overlapping conformances, and how
  same-named messages with different signatures from different parents form an
  overload set.

#### TD-06: Conformance signature matching

- **Status:** Implementation choice; open question.
- **What the paper says.** Messages carry no receiver (lines 1098-1116), and a
  compiler "will simply skip" passed-object arguments when checking conformance
  (lines 1311-1317).
- **Why it is not tight.** Beyond skipping the receiver, the paper does not
  define when an implementation matches a message: which dummy attributes must
  match, whether an implementation may be more general than the message, how
  named `pass(arg)` receivers and `nopass` relate to a message, and which dummy
  names keyword arguments use in calls through a trait.
- **Current rule.** The receiver is the first dummy, a named `pass(arg)` dummy
  in any position, or absent (`nopass`); it must have the implementing type,
  `intent(in)` or `intent(inout)` (TD-10), and neither POINTER nor ALLOCATABLE.
  The other dummies must match the message in number, type, kind, rank, intent,
  OPTIONAL and VALUE; the function or subroutine category and the result type
  and rank must match. If the message is PURE or ELEMENTAL, the implementation
  must be too. A generic message's binders must correspond with nominally
  equivalent constraints (TD-23). Dummy names may differ; calls through the
  trait keep the trait's dummy names.
- **Implementation status.** Implemented.
- **Specification action.** Specify the matching relation, including
  attributes, dummy names for keyword arguments, and whether a more general
  implementation (for example one that is PURE where the message is not, or
  that accepts more kinds) conforms.

#### TD-07: Visibility of retroactive conformances

- **Status:** Approved semantic decision (a deliberate deviation from the
  paper); open question upstream in the visibility issue,
  [difference-scheme/Traits-for-Fortran#2](https://github.com/difference-scheme/Traits-for-Fortran/issues/2).
- **What the paper says.** `implements` statements for one type may be
  distributed among different modules and files (lines 1394-1400), and a type's
  conformance may be acknowledged "even from a different module" (lines
  1319-1338). Once a type has been augmented, the new functionality is
  available on all of its instances, and importing an intrinsic type's kind
  constant (`real64` in `printy.f90`) or a derived type's original definition
  is sufficient to use it (lines 1428-1440).
- **Why it is not tight.** Under separate compilation, a compiler processing a
  client that imports only `real64` from `iso_fortran_env` has no declaration
  of `output`, of its trait or of its implementation, so it cannot resolve or
  type-check `y%output()`. The paper gives no discovery mechanism, no rule for
  which providers belong to a program, and no rule for conflicting
  implementations in independent modules. Its discussion of witness tables
  concerns generic dispatch, not the discovery of otherwise unknown
  implementations.
- **History and options.**
  1. 2026-10-04, planning: start with explicitly imported implementation
     modules and keep the paper's broad visibility as an unresolved milestone
     (chosen); require the broad visibility from the start; or leave both open.
  2. 2026-10-09, `printy.f90`: the implementers proposed automatic discovery
     within the current compilation unit, keeping explicit imports across
     separate compilations, or also automatic cross-file discovery. The
     maintainer asked for a detailed example, then rejected implicitly
     importing other modules when compiling the main program, and asked for
     alternatives closer to existing Fortran, accepting a deviation from the
     paper.
  3. The alternatives presented (also listed in the visibility issue) were:
     (1) explicit `USE` of the module that provides the implementation; (2) a
     conformance as a named, importable entity, selectable with `USE, ONLY`;
     (3) explicit facade modules that re-export a chosen set of extensions,
     complementing (1) or (2); (4) restricting where a conformance may be
     declared, to the module of the trait or of the type; (5) no extension
     methods on intrinsic types, using ordinary procedures or wrapper types
     instead. The implementers recommended starting with (1), supporting
     facades, and considering (2) for precise `ONLY` control. They withdrew
     the same-compilation-unit discovery as a language rule: placing unrelated
     program units in one file must not change what they can see.
  4. The maintainer asked for an issue in the proposal repository, filed on
     2026-10-09 as the visibility issue, and chose explicit provider `USE` to
     unblock the example.
- **Choice.** Explicit provider `USE`: a client must `USE` the module that
  provides a retroactive conformance, directly or through a facade module that
  re-exports it. Combined and separate compilation have identical visibility.
- **Consequences in the prototype.**
  - Acceptance copies add exactly one `USE` to the main program:
    `use real64_module` in `printy.f90`, `use real_type` in `overloading.f90`
    and `use intrinsics` in `ocp.f90`. The unmodified originals are kept, and
    are rejected even when the provider's module file is available.
  - An unrestricted `USE` and an explicit facade re-export import the public
    conformance records. An `ONLY` list naming only unrelated entities, or an
    empty `ONLY` list, imports no intrinsic conformance. `USE m, ONLY: T` keeps
    the public conformances of `T` that `m` exports. No named conformance
    syntax was introduced.
  - Conflicting accessible implementations are diagnosed, and the conformance
    selected at an instantiation or at the formation of a trait object is
    preserved (TD-08).
- **Upstream discussion after the choice.** In the visibility issue the
  proposal author agreed to require explicit module dependencies
  ([comment](https://github.com/difference-scheme/Traits-for-Fortran/issues/2#issuecomment-6087519036)),
  then clarified that importing the abstraction (the trait `IPrintable`)
  explicitly is acceptable, but that programmers should not be forced to import
  the implementation of `output`
  ([comment](https://github.com/difference-scheme/Traits-for-Fortran/issues/2#issuecomment-6089672348)).
  A follow-up question there, two incompatible implementations of one trait for
  the same type in different modules of one program, led to Rust and Swift
  comparisons and to the "most restrictive" plan of the restrictive-plan PR,
  [difference-scheme/Traits-for-Fortran#3](https://github.com/difference-scheme/Traits-for-Fortran/pull/3).
  That PR was merged on 2026-10-11 as examples and design documentation,
  leaving `traits.tex` unchanged. Its plan makes conformances globally unique
  and owned by the module that defines the trait or the type, discovered
  through explicit dependencies, with private implementation procedures. The
  proposal author called starting with stricter rules and relaxing them later
  sound, and kept the visibility issue open because no alternative design is
  settled. The prototype currently implements explicit provider `USE`, not
  that plan.
- **Implementation status.** Implemented for derived and intrinsic types.
- **Specification action.** Choose a visibility model: explicit provider `USE`,
  importing the trait only (the author's stated preference, which then needs a
  rule telling a compiler where the implementation is under separate
  compilation), an ownership rule with global uniqueness (the restrictive-plan
  PR), or named conformances. In each case define: identical rules for
  combined and separate compilation; scope, accessibility, host association,
  `ONLY`, renaming and re-export of conformances; diagnostics for conflicting
  accessible implementations instead of selection by import, file or link
  order; and preservation of a selected implementation through generic
  instantiation and trait objects.

#### TD-08: Selecting among visible conformances

- **Status:** Implementation choice; open question (depends on TD-07).
- **Why the paper is not tight.** The paper's ambient visibility suggests one
  program-wide set of augmentations, which "prevents the occurrence of any
  ambiguities" (lines 1428-1440), but it gives no rule for two modules that
  both provide a conformance of one type to one trait. With scoped visibility,
  different scopes of one program can see different conformances (the example
  in the visibility issue does this with modules `plain_m` and `fancy_m`). The
  paper does not say which conformance generic code and trait objects use.
- **Current rule.** A conformance is selected where a concrete type meets a
  constraint or a trait object is formed: at a generic call, at the use of a
  generic-type instance, or at a conversion to `class(Trait)`. The selection
  travels with the call, instance or object as explicit evidence; a callee
  never searches for another implementation.
  - Conflicting conformances visible in one scope are diagnosed.
  - When a generic body passes a concrete type argument to a generic message,
    the conformance visible there, in the generic definition, is used by every
    instantiation; a client can neither supply nor replace it. A type parameter
    forwarded from the caller uses the caller's selection.
  - A generic-type instance records the conformances selected where it is used,
    so two procedures that see different conformances of one argument type use
    two different instances (TD-25).
  - A converted trait object keeps the implementations selected when it was
    formed, even in a client that sees another conformance (TD-12).
- **Implementation status.** Implemented.
- **Specification action.** Either require globally unique conformances, which
  makes selection trivial (the restrictive-plan PR), or specify these selection
  points and their consequences for type identity.

#### TD-09: Static operator bindings on concrete values

- **Status:** Implementation choice (not asked).
- **Question.** Can operators bound with
  `procedure, static :: operator(*) => f, g` in an `implements` statement be
  used as ordinary defined operators on values of the concrete type outside
  generic code (for example `MyType(3) * 4`), or do they only satisfy
  requirements in generic code?
- **Paper.** Static methods are bound to the type, take no passed object and
  are always dispatched statically (lines 2740-2745); `initial ::` overloads
  the structure constructor (lines 1285-1290). Direct use of bound operators on
  concrete values is not discussed.
- **Current rule.** Where the conformance is visible, its static operator
  bindings also act as defined operators of the type when no ordinary operator
  applies. Intrinsic operators are never redefined. Operator bindings and
  `static` are accepted only in `implements` statements; in a type definition,
  `generic :: operator(*) => f` and NOPASS keep their standard meanings.
- **Implementation status.** Implemented.
- **Specification action.** State whether operator bindings in `implements`
  statements define operators of the type, how their visibility follows the
  conformance's visibility (TD-07), whether `static` is allowed in a type
  definition, and how static procedures that are not operators are referenced.

#### TD-33: Constructor interfaces as initializer implementations

- **Status:** Approved semantic decision. It is a working rule for which
  procedures implement an initializer requirement, not merely a consequence of
  how the compiler stores constructors.
- **What the paper says.** An `initial ::` declaration overloads the type's
  structure constructor and is used to check conformance to traits with
  initializer signatures (lines 1285-1295). Initializers that a type already
  has satisfy a later acknowledgement of a trait, also from another module
  (lines 1319-1338). For a cast `T(0)`, the initializer "could either be the
  type's default 'structure constructor', or a user-provided initializer"
  (lines 2045-2050).
- **Why it is not tight.** Standard Fortran already lets a module overload a
  type's constructor with a generic interface named like the type:
  `interface MyType` with module procedures that return `type(MyType)`. The
  paper does not say whether such an existing overload counts as an initializer
  when a trait with initializer requirements is adopted or acknowledged, or
  whether only procedures declared with `initial ::` do.
- **Options presented (2026-10-12).** Allow matching standard constructor
  interfaces as initializer implementations (chosen); or require an explicit
  `initial ::` binding, keeping that distinction in the compiler's
  representation.
- **Choice.** A specific procedure of a standard constructor interface named
  like the type implements an initializer requirement, like a procedure
  declared with `initial ::`, when its result is exactly the type
  (`type(MyType)`) and its signature matches the requirement.
- **Consequences.**
  - Nominal conformance is still required: the type must adopt or acknowledge
    the trait explicitly. A matching constructor interface never makes a type
    conform by itself.
  - The implicit structure constructor is not covered by this decision;
    whether it can satisfy an initializer requirement remains open (OQ-05).
  - LFortran represents `initial ::` bindings and such interfaces alike, as one
    generic constructor interface of the type, but the rule is a decision about
    the language, not a side effect of that representation.
- **Implementation status.** Implemented, together with the paper's rule for
  existing initializers (inline-06, inline-09). The specifics of a standard
  constructor interface named like the type and declared in the program unit
  that declares it, private ones included, implement a matching initializer
  requirement when their result is exactly `type(MyType)`; an acknowledgement
  in another module references them without modifying the type's module.
  Nominal conformance is still required, and the implicit structure
  constructor is not counted (OQ-05).
- **Specification action.** List the procedures that can implement an
  initializer requirement: `initial ::` procedures, matching specifics of a
  standard constructor interface (this decision), and possibly the implicit
  structure constructor (OQ-05). Also say whether constructor overloads
  declared outside the type's own program unit count.

### C. Receivers and trait objects

#### TD-10: Receiver effects

- **Status:** Approved semantic decision, in two steps; mutation through
  generic code and trait objects remains an open question.
- **Why the paper is not tight.** Messages carry no receiver (lines 1098-1116),
  so a trait cannot say whether a message may modify its receiver. In
  `vector.f90`, the message `append(item)` is implemented by
  `append(self, item)` with `type(Vector{U}), intent(inout) :: self`; the
  other examples use `intent(in)` receivers. The paper discusses neither
  receiver mutability nor pointer and allocatable receivers, nor what generic
  code and trait objects may assume.
- **Step 1 (2026-10-05).** Options: a read-only default with mutation intent
  declared explicitly per method (chosen); keep the prototype read-only and
  defer the design; a mutable default, in which callers generally need a
  definable object. The question noted that INTENT(IN) follows Fortran rules
  and is not a deep-immutability guarantee for pointer targets. The syntax for
  declaring mutation was never designed, and the early implementation was
  read-only.
- **Step 2 (2026-10-11, `vector.f90`, tracked as PAPER-D07).** Options: allow
  mutation on direct concrete calls and reject mutating bindings on read-only
  trait paths (chosen); design explicit mutating-message syntax for generic and
  runtime use and approve an edit of the example; keep the strict read-only
  rule and defer Vector.
- **Consequences.**
  - An implementation may take its receiver with `intent(inout)`. Such a
    binding serves direct calls on concrete objects under the ordinary Fortran
    rule that the passed object must then be definable.
  - Generic trait calls and trait objects stay read-only: selecting a mutating
    binding for a constraint's requirement, or forming a trait object from a
    conformance with one, is diagnosed.
  - Pointer and allocatable receivers remain unsupported, and `intent(out)`
    receivers are rejected.
  - The effect is not new syntax: it is the intent of the binding's
    passed-object dummy. `vector.f90` is accepted unchanged.
- **Implementation status.** Implemented. Mutating generic algorithms are
  unsupported.
- **Specification action.** Decide whether mutation is part of a message's
  contract. Possible directions include a mutation marker in trait messages
  (the step 1 direction, whose syntax is undesigned), deriving the effect from
  the implementations (which defeats checking generic code against the trait
  alone), or forbidding mutation through generic code and trait objects. Also
  specify pointer, allocatable and `intent(out)` receivers.

#### TD-11: Concrete `SELECT TYPE` inspection of trait objects

- **Status:** Approved semantic decision.
- **Why the paper is not tight.** The paper does not discuss `SELECT TYPE` on
  trait objects.
- **Options presented (2026-10-05).** Permit ordinary concrete inspection
  (chosen), or keep trait payloads opaque and defer inspection. The question
  concerned `TYPE IS (concrete type)` and `CLASS IS (actual base type)`, not
  `CLASS IS` with an unrelated trait.
- **Consequences.** `TYPE IS` uses the exact nominal concrete type, including
  kind parameters. `CLASS IS` follows only the concrete type's real extension
  chain; the concrete parent need not implement the trait. A trait name in
  `TYPE IS` or `CLASS IS` is diagnosed, because inspection is not a query for
  conformance to another trait. `CLASS DEFAULT` keeps the object's trait
  contract. The associate name follows the ordinary Fortran rules.
- **Implementation status.** Implemented for scalar trait objects.
- **Specification action.** Specify `SELECT TYPE` on trait objects, including
  whether a dynamic conformance query such as `CLASS IS (ITrait)` exists (the
  prototype says no) and what it would require under separate compilation.

#### TD-12: Anonymous combinations and conversions between trait objects

- **Status:** Approved prototype option (scope); the conversion rules are
  implementation choices.
- **Why the paper is not tight.** `sect:trait_combinations` (lines 1652-1683)
  introduces `class(ICastable + IPrintable)` and says that such an object
  conforms to both traits. It does not define assignment and pointer
  association between differently declared trait objects: weakening a child to
  a parent or a combination to a subset, whether `A + B` and `B + A` are the
  same, or how a named child trait extending `A + B` relates to the anonymous
  `A + B`.
- **Options presented (2026-10-07).** Implement general combinations now,
  including conversions to combinations declared only by later, separately
  compiled clients (chosen); or first a bounded slice (direct construction,
  reordered combinations and projections to members), diagnosing more general
  conversions.
- **Current rules.** `class(A + B)` is an anonymous conjunction of nominal
  contracts. Order and repetition do not matter, and redundant ancestors are
  normalized away. A named child extending `A + B` is distinct from `A + B`: it
  can be weakened to `A + B`, but knowing both parents never establishes the
  child. An existing object can be weakened to any parent or subset
  conjunction, including one declared only by a later client, without
  rebuilding the provider, and it keeps the implementations selected when it
  was formed. Allocatable dummies and defining pointer dummies stay invariant
  for every intent, because they associate storage, not values.
- **Implementation status.** Implemented for scalar trait objects.
- **Specification action.** Specify conversions between trait objects
  (weakening, equivalence, invariance of allocatable and pointer dummies) and
  the meaning of combinations as declared types.

#### TD-13: Initializer requirements in runtime views

- **Status:** Approved semantic decision.
- **Why the paper is not tight.** The paper classifies all traits of its first
  extension chapter as universal (lines 1971-1976), including `ICastable` and
  `IConstructable`, which contain initializer signatures, and it declares
  `class(IAdmissible)` and `class(ICastable + IPrintable)` objects (inline-12
  and inline-13, lines 1652-1673). An initializer has no receiver, however, and
  cannot be sent as a message to an object. The paper does not say what an
  initializer requirement means for a trait object, for example whether it
  allows constructing a new value of the object's dynamic type.
- **Options presented (2026-10-12).** Check initializer conformance but omit
  constructors from runtime dispatch slots (chosen); or a different runtime
  view design.
- **Consequences.** Initializer requirements remain obligations that are
  checked when a nominal conformance is established. A trait object's dispatch
  table contains only ordinary messages, which remain callable through it. This
  does not make traits with `itself` operator requirements or with associated
  types usable as trait objects; the paper restricts those to generic
  constraints (lines 1977-1992). The paper's separate rule that existing
  initializer bindings satisfy a later acknowledgement is explicit (see "Rules
  the paper already states"); which procedures can implement an initializer
  requirement is recorded in TD-33.
- **Implementation status.** Implemented: a trait whose only requirements
  without a receiver are initializers declares trait objects (inline-12 and
  inline-13). Its initializer requirements are checked when a conformance is
  established and have no slot in the view, so a view of `ICastable` alone has
  none and is inspected with `SELECT TYPE`. Views stay read-only (TD-10), and
  traits with operator requirements or associated types stay generic
  constraints only.
- **Specification action.** State whether initializer requirements are part of
  a trait object's interface. If not, say that they are conformance-only. If
  they are, define how a value of the dynamic type is constructed through a
  trait object. Also confirm that an initializer requirement alone does not
  make a trait one with associated types. Related: which procedures implement
  an initializer requirement of a derived type. Procedures bound with
  `initial ::` do, and by the approved rule of TD-33 so do matching specifics
  of a standard constructor interface named like the type. Whether the
  implicit structure constructor can, as the paper suggests for a cast `T(0)`
  (lines 2045-2050), is still open (OQ-05).

#### TD-14: Lifetime of trait-object function results

- **Status:** Approved prototype option (implementation).
- **Question (2026-10-07).** A function returning a trait object (a factory)
  can be referenced several times in one array expression. Each result must
  stay alive until the enclosing construct completes, but the implementation
  reused one temporary.
- **Options presented.** Implement general temporary retention now (chosen), or
  diagnose such contexts for now and implement retention next.
- **Implementation status.** Implemented.
- **Specification action.** None identified: ordinary Fortran function-result
  semantics apply.

### D. Generic procedures, methods and type sets

#### TD-15: Runtime generic methods over closed type sets

- **Status:** Approved prototype option (implementation strategy). The dispatch
  layout is not proposed for standardization.
- **Question (2026-10-10).** `mixed.f90` and `static.f90` call the generic
  method `sum{INumeric :: T}`, with `INumeric` the type set
  `integer | real(real64)`, through `class(ISum)` and `class(IAverager)`
  trait objects. How should a generic method be dispatched on a trait object?
- **Options presented.** Provider-owned entries for each member of the closed
  set now, extending the open-world path for arrays and results for `ocp.f90`
  later (chosen); or extend the general open-world ABI for arrays and `T`
  results first, before finishing `mixed.f90` and `static.f90`.
- **Semantic requirement versus implementation.** From the paper (lines
  1708-1712 and table `tab:dispatch`, line 2612), the type argument is
  substituted at compile time and only the provider behind `class(...)` is
  chosen at run time. LFortran realizes this with one dispatch slot per member
  tuple of the type set, filled by the provider with entries compiled once in
  the provider's module; a client selects the slot from its static type
  argument and never instantiates a provider body. Adding a member to a type
  set changes the trait and requires recompiling its dependents, a cost the
  paper accepts for type sets (`sect:Go_encapsulated`). By contrast, `ocp.f90`
  needs genuinely open dispatch, which is required and is now implemented
  (TD-16); "later" in the chosen option refers only to the order of work.
- **Implementation status.** Implemented; `mixed.f90` and `static.f90` are
  accepted.
- **Specification action.** State the dispatch semantics of generic methods on
  trait objects (static type argument, dynamic provider) explicitly, and leave
  the mechanism to processors.

#### TD-16: Open protocols at run time (`ocp.f90`), and static-only providers

- **Status:** Approved prototype option (a policy for implementation limits).
  Runtime dispatch for `ocp.f90` itself is required and is implemented; it
  was not deferred.
- **Background.** In `ocp.f90` (`sect:ocp_conformance`), `INumeric` is a trait
  with the associated type `itself` (an initializer and operators), and
  `integer` and `real(real64)` acknowledge it with empty `implements`
  statements. `ISum` is a universal trait whose generic method
  `sum{INumeric :: T}` has its binder constrained by that `INumeric`, and the
  program calls it through `class(ISum)` trait objects. The paper says that
  adding a module that implements `INumeric` for a new type is sufficient, and
  that no pre-existing code needs to be modified (lines 2686-2699).
- **Two different trait objects.** `class(INumeric)` is invalid: a trait with
  associated types is a generic constraint only (lines 1977-1992), and the
  prototype keeps that restriction. `class(ISum)` is a valid trait object of a
  universal trait; only the binder of its generic method is constrained by
  `INumeric`. Runtime dispatch through `class(ISum)`, and with it the complete
  `ocp.f90`, are required for the prototype and are implemented.
- **Implementation approach.** A provider's generic method is compiled once,
  and each call passes a table of the operations of `T`, so that types added
  later work without rebuilding the provider. This mechanism is LFortran's
  choice, not a proposed requirement (OQ-14).
- **Question (2026-10-12).** Some valid providers have generic-method bodies
  that use operations outside the subset this runtime path supports so far.
  The candidate implementation rejected such providers where they were
  defined, even when they were used only statically.
- **Options presented.** Preserve static-only use and diagnose only an
  attempted runtime use (chosen); or reject such providers at definition time
  for now.
- **Consequences.** Only these additional body shapes are diagnosed, and only
  where a runtime use of them is requested; their static uses must keep
  working. A limit of the runtime path never invalidates otherwise valid static
  code.
- **Implementation status.** Implemented. `ocp.f90`, with its approved
  `use intrinsics` (TD-07), runs for both choices in normal and fast modes,
  and the original without that line is rejected with a located diagnostic.
  Each algorithm library is compiled once, with one open entry per generic
  method that takes the caller's table; `intrinsics` owns the tables of
  `integer` and `real(real64)`. A separately compiled check freezes all five
  library modules before compiling `main`, and another adds `Rational` and
  `real(real32)` conformances in a later module, after the provider's objects
  were frozen and hidden. The chosen policy is implemented: a provider whose
  generic-method bodies use operations outside the supported runtime subset
  stays statically usable, its module publishes no runtime entries for that
  conformance, and only a requested runtime view of it is diagnosed, where the
  view is formed. The remaining limits of the runtime path, such as array
  values computed from erased values, are diagnosed as not implemented yet
  and are temporary (see [traits](traits.md)).
- **Specification action.** None for the rule itself. See OQ-14 on which
  separate-compilation properties are normative.

#### TD-17: Kind wildcards

- **Status:** Open question.
- **What the paper says.** `integer(*) | real(*) | complex(*)` includes all
  integer, real and complex types admitted by the language; `real(*)` means the
  real kinds that "the particular Fortran implementation supports" (lines
  1804-1828; inline-17).
- **Why it is not tight.** A wildcard makes a trait's member set
  processor-dependent. The paper does not say whether kinds with partial
  support count (for example an interoperability-only `real(10)`), whether the
  set may depend on the compilation target, or how this interacts with checking
  generic bodies against every member, with runtime dispatch slots (TD-15) and
  with module files.
- **History.** On 2026-10-05 the implementers asked how to prioritize
  wildcards, given that ordinary declarations accept `real(10)` while its
  conversion, target and serialization support is inconsistent. The options
  were: defer full `real(*)` while doing integer and complex wildcards; fix
  `real(10)` support first; defer all wildcards. The maintainer chose none,
  asking to settle runtime polymorphism first (TD-01).
- **Implementation status.** This branch diagnoses kind wildcards as not
  implemented yet. They are being implemented as shorthand for the processor's
  kinds; the candidate list leaves out the interoperability kind `real(10)`.
  That list has not been approved.
- **Specification action.** Define the member set of a wildcard, for example as
  the kinds listed in `INTEGER_KINDS`, `REAL_KINDS` and similar constants of
  `ISO_FORTRAN_ENV`, and say whether code checked against a wildcard set must
  remain valid on processors with other kinds.

#### TD-18: The intrinsic `generics` module

- **Status:** Open question.
- **What the paper says.** The language "should ideally" supply predefined
  traits in an intrinsic module, "tentatively" called `generics`, possibly
  including `IAnyType`, numeric traits and traits for relational operators;
  inline-20 uses `use, intrinsic :: generics, only: INumeric` (lines
  1911-1948).
- **Why it is not tight.** The module's name is tentative, and its contents and
  their definitions are not given. For example, `INumeric` appears in the paper
  both as a type set (`integer | real(real64)`, with or without wildcards) and
  as a trait with associated types (`ocp.f90`).
- **Implementation status.** This branch has no such module, so inline-20 is
  not yet accepted. A module exporting a type-set `INumeric` with
  `integer(*) | real(*) | complex(*)` is being implemented as a candidate; its
  contents have not been approved.
- **Specification action.** Fix the module's name and specify its complete
  contents and definitions.

#### TD-19: The empty trait and unconstrained type parameters

- **Status:** Implementation choice; open question.
- **What the paper says.** An omitted constraint means the empty trait
  `IAnyType` (lines 1722-1724), which "matches all types (since any type has at
  least zero methods)" (lines 1830-1839). `vector.f90` declares `Vector{U}`
  without a constraint, stores `U` values in an allocatable array, builds
  arrays with `[item]` and `[self%elements, item]`, and prints them with
  `print *, self%elements`. Generic bodies are fully type-checked at
  definition ("strong concepts", lines 1722-1726).
- **Why it is not tight.**
  - Is a declared empty abstract interface a nominal trait that a type must
    adopt, like every other universal trait, or does it match every type
    structurally, as the quoted sentence suggests for `IAnyType`?
  - Which operations does an unconstrained parameter allow: intrinsic
    assignment, allocation, array constructors, list-directed input and output?
    "Zero methods" suggests none, yet the paper's own `Vector` uses all of
    them. Printing cannot be checked at definition time for every possible `U`
    (for example a derived type with a pointer component or with a defined
    output procedure).
- **Current rule.** An unconstrained parameter admits any type and its
  instances record no conformance. Its body may use storage, assignment, array
  constructors and input/output; input/output is checked per instance (TD-27).
  The prototype represents `IAnyType` by an unconstrained parameter; it does
  not define a structurally matching empty trait.
- **Specification action.** Define `IAnyType` precisely, the operations
  available on every type parameter, and when they are checked.

#### TD-20: Associated types

- **Status:** Implementation choice; open question.
- **What the paper says.** A trait declares an associated type with a type
  definition statement, `type :: Element` in `IAppendable`, "left for inference
  by the compiler" (lines 1864-1910); for `Vector{U}` the compiler infers
  `Element` to be `U` from `append`'s implementation (lines 2766-2802).
  `itself` is a predefined associated type (lines 1878-1890).
- **Why it is not tight.** There is no inference rule for an associated type
  used by several messages, or by none; no statement whether it is one type per
  conformance or may vary per call; no syntax for naming another type's
  associated type in generic code; no rule for constraining an associated type;
  and no description of how a trait with associated types is used as a generic
  constraint.
- **Current rule (not put to the maintainer as a question).** Each conformance
  binds an associated type once, from its first use in a binding, and every
  other use must agree. An associated type that no message uses is diagnosed. A
  generic message whose result is of an associated type must return one type
  per implementation, not a type parameter of its own. A character binding
  needs a constant length. For `Vector{U}`, `U` is fixed per instance, so each
  instance binds `Element` to its own type argument. A trait with associated
  types is a generic constraint only.
- **Implementation status.** Implemented as described. Naming another type's
  associated type, constraining generic procedures or types by such traits, and
  generic messages with a dummy argument of an associated type are not
  implemented yet.
- **Specification action.** Specify inference, the one-type-per-conformance
  rule, constraints on associated types, and a projection syntax for generic
  code if needed.

#### TD-21: Character type arguments

- **Status:** Implementation choice (recorded as an assumption, not asked).
- **Question.** Does a type argument inferred from a character value carry its
  length? `Vector("John ")` stores five-character names, and the paper's output
  `John Mary Anne ` requires five-character storage.
- **Current rule.** A type argument is the complete type including kind and
  constant length: `Vector("John ")` is `Vector{character(len=5)}`, and
  `Vector{character(len=2)}` is another instance. Nonconstant and deferred
  lengths are diagnosed as a limit of the prototype.
- **Implementation status.** Implemented.
- **Specification action.** Say whether length (and other) type parameters are
  part of a type argument, and whether assumed or deferred lengths can be type
  arguments.

#### TD-22: `:=` declarations in loops

- **Status:** Implementation choice (one of the design gates of TD-01; not put
  to the maintainer as a question).
- **What the paper says.** `:=` declares and initializes local variables, also
  loop indices: `do i := 1, 10` with `tmp := real(i)` in the body, and
  `arr := [(real(i), i := 1, 10)]`; module variables and function results
  cannot be declared this way (lines 2054-2084).
- **Why it is not tight.** The scope and lifetime of a `:=` loop index and of a
  `:=` declaration inside a loop body or conditional code are not defined, nor
  whether a name can be redeclared.
- **Current rule.** A `DO` index or body variable declared with `:=` belongs to
  the containing procedure, main program or `BLOCK`, not to a new loop scope;
  it cannot redeclare a local name, and the assignment executes on each
  applicable iteration. An implied-`DO` index declared with `:=` is local to
  its implied-`DO`, like an array-constructor index. An inner `BLOCK` may
  shadow an outer name.
- **Implementation status.** Implemented, see [usage](../usage.md).
- **Specification action.** Define scope, lifetime and redeclaration rules for
  `:=`, including loops and conditional code.

#### TD-23: Constraint matching and type-set capabilities

- **Status:** Implementation choices; open questions.
- **Why the paper is not tight.**
  - It does not say how an implementation's generic method must relate to the
    trait's generic signature (`sect:generic_interfaces`): equal constraints,
    or also weaker ones.
  - Inline type sets are "an alternative notation" for a type-set trait (lines
    1841-1862), but it is not said whether two inline sets with the same
    members are the same constraint.
  - A type set's operations are "the intersecting (common) set of all the
    operations and intrinsic procedures" of its members (lines 1784-1802), but
    the paper does not say whether mixed-mode operations with non-`T` operands
    (`x + 1`), unary operators, whole-array operations and intrinsic procedures
    belong to it, or what their result types are.
  - It does not say whether a type set can be combined with `+` with another
    type set or with a universal trait, or extended.
  - Fortran has no reserved words, so `{integer :: T}` could name either the
    intrinsic singleton or a visible trait called `integer`.
- **Current rules.**
  - A generic message's implementation may rename its binders, but its
    constraints must be nominally equivalent to the message's, neither stricter
    nor weaker.
  - Each inline type set is its own anonymous trait, private to one binder of
    one definition; equal member lists are not merged. Forwarding between
    distinct type sets, even with equal members, is not supported, and an
    implementation's own inline set is not equivalent to a message's inline
    set. No paper example implements a message declared with an inline set
    (inline-19 is only a declaration).
  - Generic bodies constrained by a type set are checked against every member.
    Scalar `+`, `-`, `*`, `/` and comparisons need operands of the same type
    parameter. `T(x)` is the intrinsic conversion and `size` is available;
    other intrinsic procedures, unary operators and whole-array arithmetic are
    not implemented yet.
  - A bare intrinsic-looking name such as `integer` denotes a visible named
    trait if there is one, and otherwise the intrinsic singleton; an explicit
    kind or a union always denotes intrinsic members.
- **Implementation status.** Implemented as described; combining type sets with
  each other or with universal traits is not implemented yet.
- **Specification action.** Specify constraint equivalence for generic method
  conformance, the identity of inline type sets, the exact capability set of a
  type set including result types, and composition of type sets.

### E. Generic derived types

#### TD-24: Generic `implements` headers, binder scopes and procedure ownership

- **Status:** Approved prototype option (temporary limits with explicit
  diagnostics, not permanent language rules).
- **What the paper says.** `implements ISum :: PairwiseSum{ISum :: U}` repeats
  the type's parameter list (inline-32, lines 2250-2284). A generic type's
  parameters are accessible in its bound initializers and procedures, and a
  method can combine the type's `U` with its own `T` (lines 2293-2301).
  Initializers inherit the type's parameters and must not declare them (lines
  2343-2347).
- **Why it is not tight.** The paper does not say whether the `implements`
  header must repeat the list exactly, may rename binders, may omit the list or
  may state stronger constraints (conditional conformance); whether method
  binders may shadow the type's parameters, or initializers have parameters of
  their own; whether a procedure that uses the type's parameters may be called
  by name, imported, or bound to two generic types; and whether a procedure
  that does not use them may be bound to a generic type.
- **Options presented (2026-10-10).** For the header: exact repetition for now
  (chosen), supporting renamed binders with equivalent constraints, or also
  inferring an omitted list. For binder scopes: no method shadowing and no
  extra initializer parameters for now (chosen), or full lexical scopes. For
  parameter-dependent procedures: one owning generic type, reached only through
  its bindings and constructors for now (chosen), or direct calls and sharing
  between types. The implementers noted that the rules cover the paper examples
  without committing to permanent restrictions, and that the alternatives would
  be diagnosed, not given different meanings.
- **Consequences.** These limits are diagnosed. A procedure that does not use
  the type's parameters is not owned by the generic type: it remains an
  ordinary module procedure with normal Fortran behavior, which can be called
  by name and imported. Binding such a procedure to a generic type, however, is
  a separate limit and is diagnosed as not implemented yet. In addition, an
  `implements` statement for a generic type must be in the type's module.
- **Implementation status.** Implemented with diagnostics.
- **Specification action.** Specify each point, conditional conformance in
  particular, binding procedures that do not use the type's parameters, and
  whether a generic type can conform retroactively in another module.

#### TD-25: Identity and ownership of generic instances

- **Status:** Implementation choice with temporary limits; open question.
- **What the paper says.** `PairwiseSum{U}` "really refers to a set of
  multiple, related, but different" types (lines 2303-2316), and explicit and
  inferred constructor calls "give the same result" (lines 2385-2393).
- **Why it is not tight.** The paper does not define when two instances are the
  same type: whether `PairwiseSum{SimpleSum}` named in two modules is one type
  (a Fortran derived type normally has one definition), whether the selected
  conformance is part of the identity (TD-08), which program unit owns an
  instance's code and runtime tables under separate compilation, or whether
  generic types can be extended and their generic bindings overridden.
- **Current rules.**
  - An instance is an ordinary sealed derived type owned by the module or main
    program that creates it. Equal instances in one unit are one type.
  - Instances are not shared between program units yet: equal instances created
    by different units are different types, and an instance's conformances are
    visible only in its unit. The design intends one identity across units when
    the selected conformances agree; this is planned work, not a decision that
    such instances differ.
  - The selected conformances are part of an instance (TD-08).
  - Instance entities have names that no source can spell and are not imported
    by `USE`. Compiling a client never changes the files of the module that
    defines the generic type.
  - Generic types and their instances cannot be extended, overriding a binding
    to a generic procedure is not implemented yet, and generic types cannot
    have FINAL, GENERIC or DEFERRED bindings yet.
- **Implementation status.** Implemented as described.
- **Specification action.** Define instance identity, including the role of the
  selected conformances, ownership expectations under separate compilation, and
  extension and overriding of generic types and generic bindings.

#### TD-26: Host association of generic instances

- **Status:** Approved prototype option (temporary limit).
- **Question (2026-10-12).** An internal procedure of a main program that uses,
  by host association, a program variable whose type is (or contains) a generic
  instance created by the program crashed the compiler. Supporting it requires
  moving the instance's type, procedures and conformance evidence together. The
  paper's `static.f90` does not use this pattern.
- **Options presented.** A precise diagnostic now and full support after the
  remaining paper examples (chosen), or full support now.
- **Consequences.** Such a use is diagnosed as not implemented yet. Passing the
  variable as an argument, declaring the instance in the internal procedure,
  and host association of an instance created by a module all work.
- **Implementation status.** Diagnosed.
- **Specification action.** None. This is valid Fortran that a complete
  implementation must accept; it is recorded so that the limit is not mistaken
  for a language rule.

#### TD-27: Unused bindings of generic instances and defined I/O

- **Status:** Approved prototype option (temporary limit); open question.
- **Background.** The prototype copies every procedure of a generic type into
  each instance, whether or not it is called. `vector.f90`'s `Vector{U}` has a
  `printout` binding that prints its `U` items, so an instance for a type whose
  items cannot be transferred by default input/output (for example a derived
  type with an allocatable component) failed during code generation, even if
  `printout` was never called.
- **Options presented (2026-10-12).** A located temporary diagnostic now,
  deciding the general policy after the paper examples (chosen); checking and
  instantiating only the bindings that are used; or requiring every binding to
  be valid for every instance, with an early semantic error.
- **Consequences.** An instance whose copies need an unsupported transfer is
  rejected where it is created, even if the binding is never called. This eager
  rejection is temporary, not a language rule; checking only used bindings is
  not implemented either.
- **Related limit (not asked separately).** Instance copies also ignored a type
  argument's defined input/output procedure and printed the default transfer, a
  silently wrong result. The implementers applied the same approved
  temporary-diagnostic limit: such transfers are now diagnosed. This
  application was not put to the maintainer as a separate question. Defined
  input/output in instances is deferred together with the general policy.
- **Implementation status.** Diagnosed as described.
- **Specification action.** Decide when the bindings of a generic type are
  checked for a type argument: at the generic definition (which needs a
  constraint that provides input/output), per instance for all bindings, or
  only for used bindings; and how a type argument's defined input/output
  applies in generic code (TD-19).

#### TD-28: Reallocation on assignment in `vector.f90`

- **Status:** Approved prototype option (validation; LFortran-specific).
- **Question (2026-10-12).** `append` executes
  `self%elements = [self%elements, item]`, which relies on the Fortran 2003
  reallocation of an allocatable variable on intrinsic assignment. LFortran's
  default mode does not reallocate; its existing option `--realloc-lhs-arrays`
  requests the standard semantics.
- **Options presented.** Use the option in Vector's acceptance commands and
  keep the source unchanged (chosen), or make it work without the option before
  accepting Vector.
- **Implementation status.** The acceptance tests of `vector.f90` use the
  option; compiler defaults are unchanged.
- **Specification action.** None. This is standard Fortran and a property of
  LFortran's default mode, not a requirement of the proposal.

### F. Errata and acceptance-copy adaptations

#### TD-29: END TYPE closing statements

- **Status:** Erratum.
- **Listings.** inline-05 (lines 1250-1283), inline-07 (lines 1348-1364) and
  inline-08 (lines 1374-1393, the statement is on line 1380) close the derived
  type `MyType` with `end MyType`; inline-09 (lines 1474-1505) closes a type
  definition with `end implements MyType`.
- **Questions.** On 2026-10-08, correcting the closing statements of inline-05,
  -07 and -09 only in acceptance copies (yes). On 2026-10-12, the same
  correction for inline-08: correct it in the acceptance copy only (chosen), or
  leave it as an unresolved paper error.
- **Consequences.** Only the closing statement changes, to `end type MyType`.
  The raw listings and the paper are unchanged, and the compiler continues to
  reject mismatched closing statements.
- **Specification action.** Correct the four listings.

#### TD-30: SAXPY result declaration

- **Status:** Erratum (an interpretation chosen by the maintainer).
- **Listing.** In `ICalculable` (inline-01, lines 1071-1089), `saxpy` declares
  the result `real :: res(:)`. A function result cannot be assumed-shape; it
  needs explicit bounds or the ALLOCATABLE or POINTER attribute.
- **Options presented (2026-10-09).** `real :: res(size(x))`, a standard
  explicit-shape result (chosen); `real, allocatable :: res(:)`; or keeping
  `real :: res(:)` and designing a new rank-only rule for trait results.
- **Consequences.** The acceptance copy uses `real :: res(size(x))`, the raw
  listing is preserved, and no rank-only result extension exists.
- **Specification action.** Correct the listing, or specify a shape-polymorphic
  result if one is intended.

#### TD-31: Completed context and boilerplate

- **Status:** Approved prototype option (the general instruction of
  2026-10-09); each individual completion is an implementation choice.
- **Instruction.** Every example must get a working test; missing boilerplate
  may be added; design questions must be asked.
- **Completions.**
  - Ellipses (`...`) and missing enclosing modules, programs and declarations
    of the module fragments (inline-05 to -09, -29 and -32) are completed.
  - Explicit provider `USE` statements (TD-07) are added where a program uses a
    conformance from another module.
  - inline-28 (lines 2158-2178) instantiates `sum{real(real32)}`, while the
    `INumeric` assumed just before (lines 2115-2121) admits only `integer` and
    `real(real64)`. Its context declares `INumeric` as
    `integer | real(real32) | real(real64)`, keeping both the `real32`
    `ASSOCIATE` alias and the `real64` procedure pointer intact.
  - inline-33 (lines 2324-2329) calls methods of `pairwise` without giving it a
    value. The copy adds `pairwise = PairwiseSum(SimpleSum())` between the
    declaration and the calls. This is boilerplate, not an implicit
    default-initialization rule.
- **Specification action.** Make inline-28's constraint context explicit and
  define `pairwise` in inline-33.

#### TD-32: Spellings superseded by the updated sources

- **Status:** Resolved by the proposal update; no choice needed.
- **Details.** The older companion `ocp.f90` and `overloading.f90` declared
  initializer requirements as `function init(n)` ... `end function init`, while
  the paper and commit `5163a026` use `initial(n)` ... `end initial`. The older
  `vector.f90` declared the associated type with
  `typedef, deferred :: Element`, which the paper's wording no longer
  supported; commit `5163a026` uses `type :: Element` inside the trait. The
  planning question of 2026-10-04 had listed the associated-type spelling as an
  open design gate; the update settled it.
- **Specification action.** None beyond TD-20.

## Rules the paper already states

These were checked against the paper and needed no question. They are listed so
that they are not reopened as ambiguities.

| Rule | Paper | Prototype |
| --- | --- | --- |
| Conformance is nominal: matching names and signatures alone do not make a type conform. Structural subtyping is only an Outlook idea. | `sect:implementing_traits`; lines 2703-2713; `sect:structural_subtyping` | Implemented |
| Messages carry no receiver; passed-object arguments are skipped when checking conformance. | lines 1098-1116, 1311-1317 | Implemented (TD-06) |
| A child trait is a subtype of each of its parents. | lines 1150-1160 | Implemented |
| `implements :: T`, without traits, binds initializers and methods to a type. | `sect:adding_methods` (lines 1241-1301), inline-05 | Implemented for a derived type of the same program unit; other targets and binding forms are diagnosed as not implemented yet |
| Bindings that a type already has, including its `initial` bindings, satisfy a later acknowledgement of a trait, also from another module. | lines 1285-1295, 1319-1338; inline-06, inline-09 | Implemented (TD-33) |
| An `implements` statement cannot target an abstract type and does not accept `deferred` bindings; the type attribute can be used with abstract types, which may leave obligations to their concrete descendants. | lines 1227-1236, 1597-1603 | Implemented |
| `extends` precedes `implements` in a type definition. | line 1544 | Implemented |
| A sealed type cannot be extended, and its passed-object dummies may use TYPE instead of CLASS. | lines 1505-1513 | Implemented |
| Intrinsic operations and conversions cannot be redefined, and attempts are errors; intrinsic types acknowledge them with empty `implements` statements. | lines 1406-1415, 2686-2692, 2746-2764 | Implemented |
| Static methods take no passed object and are dispatched statically. | lines 2740-2745 | Implemented for operator bindings (TD-09) |
| Trait objects must be allocatable, pointers or dummy arguments. | lines 1643-1650 | Implemented for scalars |
| Type sets and traits with associated types are generic constraints only: they cannot declare `class(...)` objects, and type sets cannot be implemented by derived types. Universal traits can be both. | lines 1971-1992 | Implemented (initializers: TD-13) |
| Type-set membership is exact, and a type set provides the operations common to all of its members; complex members exclude ordering. | lines 1760-1802 | Implemented by checking each member (TD-23) |
| Generic bodies are fully type-checked at definition. | lines 1722-1726 | Implemented (see TD-19) |
| `T(x)` for an intrinsic `T` is the intrinsic conversion to `T`'s kind. | lines 2032-2041 | Implemented |
| Generic type parameters are inferred from ordinary arguments; explicit `{...}` arguments are allowed and are needed for procedure values. | lines 2098-2179 | Implemented |
| `:=` declares local variables only, not module variables or function results. | lines 2080-2084 | Implemented (loops: TD-22) |
| Main programs and modules cannot be generic. | lines 2088-2096 | Followed |
| Initializers of a generic type inherit its parameters and must not declare them; constructor calls infer the parameters; explicit and inferred calls give the same result. | lines 2331-2393 | Implemented |
| Adding a member to a type set changes the trait and requires recompiling its dependents. | `sect:Go_encapsulated` | Implemented (TD-15) |

## Open questions for the proposal

This backlog collects the points that the paper does not settle. Each item
names the entries with the details.

| ID | Question | See |
| --- | --- | --- |
| OQ-01 | Visibility and coherence of conformances: explicit provider `USE`, importing the trait only, an ownership rule with globally unique conformances, or named conformances; `ONLY`, renaming, re-export and conflicts; identical rules for combined and separate compilation. | TD-07, TD-08 |
| OQ-02 | The exact rule for an implementation matching a message: attributes, dummy names for keyword arguments, more general implementations. | TD-06 |
| OQ-03 | Composition: identity of requirements, overloads across parents, agreement of intrinsic acknowledgements. Overloaded requirements that coincide after substituting `itself`, such as `operator(*)` with `(itself, itself)` and `(itself, integer)` when the implementing type is `integer`. How an elemental and a non-elemental requirement that both accept a call are resolved (standard generic resolution prefers the non-elemental one; the prototype currently reports an ambiguity). | TD-05 |
| OQ-04 | Whether a message may modify its receiver, how this is declared, and what generic code and trait objects may assume; pointer, allocatable and `intent(out)` receivers. | TD-10 |
| OQ-05 | Initializer requirements and trait objects. Which procedures implement an initializer requirement: `initial ::` procedures and matching standard constructor interfaces are approved for the prototype (TD-33); whether the implicit structure constructor can is open. | TD-13, TD-33 |
| OQ-06 | Trait objects: conversions and combinations; `SELECT TYPE`, including dynamic conformance queries; arrays of trait objects (homogeneous like `CLASS` arrays or not); pointer components and results. | TD-11, TD-12 |
| OQ-07 | Associated types: inference, one type per conformance, constraints on associated types, projection syntax, use as generic constraints. | TD-20 |
| OQ-08 | Type sets: the exact common operations and their result types, mixed-mode operations, intrinsic procedures, identity of inline type sets, composition with `+`. | TD-23 |
| OQ-09 | The member set of kind wildcards and its processor dependence. | TD-17 |
| OQ-10 | Name, contents and definitions of the intrinsic `generics` module. | TD-18 |
| OQ-11 | `IAnyType` and the operations of unconstrained parameters; when unused bindings of generic types are checked; defined input/output in generic code. | TD-19, TD-27 |
| OQ-12 | Generic methods: whether implementations may have weaker constraints; overloaded generic messages that differ only in their generic parameter lists (a commented-out paragraph of the paper source, lines 1737-1742, would allow them). | TD-23 |
| OQ-13 | Generic derived types: `implements` headers and conditional conformance, binder scopes, ownership of parameter-dependent procedures, binding of procedures that do not use the parameters, retroactive conformance from other modules, instance identity across program units, extension and overriding. | TD-24, TD-25 |
| OQ-14 | Which separate-compilation properties are normative. For example: must adding a conformance for a new type work without recompiling a separately compiled generic provider (the OCP discussion, lines 2686-2699, only says no code needs to be modified), and must a constraint by a child trait forward to a callee constrained by its parent in separately compiled code? Dispatch tables, member entries and other mechanisms should remain processor details. | TD-15, TD-16 |
| OQ-15 | Scope, lifetime and redeclaration of `:=` declarations, including loops and conditional code. | TD-22 |
| OQ-16 | Whether kind and length type parameters are part of type arguments, and whether assumed or deferred lengths are allowed. | TD-21 |
| OQ-17 | Static methods that are not operators, `static` in type definitions, and operator bindings as defined operators. | TD-09 |
| OQ-18 | Whether a sealed type can be abstract. A commented-out sentence of the paper source (lines 1515-1517) says it cannot; the prototype rejects the combination. | -- |
| OQ-19 | Whether the new words are reserved and where they are recognized. | TD-03 |
| OQ-20 | Which restrictions are normative. `sect:restrictions` states that type sets and traits with associated types are generic constraints only "presently (i.e. in the framework of a prototype implementation of this proposal)" (lines 1977-1983). A specification should separate permanent rules from limits of a first implementation; the temporary limits recorded here must not become implicit prohibitions. | TD-24 to TD-27 |

## Questions that do not affect the language

Four questions concerned only the development process: how to handle an
unrelated CI failure in the C backend (a separate pull request), build
isolation for that fix (a separate build), rebasing the traits branches onto an
updated main branch (approved), and whether to start the next runtime stage
after the exhaustive CI run instead of waiting for an independent review (start
after the CI run). The first planning form also asked about internal tooling.
None of them affects the proposal.

## Adding an entry

Use the next free identifier and never renumber or reuse one. Mark a superseded
entry as such instead of deleting it. Record an option as chosen by the
maintainer only if the maintainer actually chose it, and give the options as
they were presented; if they cannot be recovered, say so. Describe the
implementation status qualitatively and keep test counts out of this document.

```markdown
#### TD-NN: Short title

- **Status:** One or more labels from the status legend.
- **What the paper says.** Section labels, line numbers and listings.
- **Why it is not tight.** What the paper leaves open.
- **Options presented (date).** As presented, or "not asked".
- **Choice.** What was chosen, and by whom.
- **Consequences.** Rationale and effects on programs.
- **Implementation status.** Qualitative.
- **Specification action.** What the proposal should say.
```
