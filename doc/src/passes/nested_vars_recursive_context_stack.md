# Nested Procedure Recursive Context Stack Design

## Scope

This document describes the nested-procedure recursion fix implemented on top of
`nested1` (the `nested1..HEAD` delta on `nested2`), focused on making the
Man-or-Boy style example (`b.f90`) correct without changing procedure
signatures.

It documents all changed areas in:

- `src/libasr/pass/nested_vars.cpp`
- `src/libasr/codegen/asr_to_llvm.cpp`
- `src/libasr/codegen/llvm_utils.cpp`
- `integration_tests/nested_24.f90`
- `integration_tests/CMakeLists.txt`


## Problem Statement

The previous nested-vars lowering used a single mutable global context per host
procedure. Recursive and re-entrant calls would overwrite that single context.
For procedure captures this caused:

- wrong closure binding across recursion levels
- wrong procedure/context pairing for indirect calls
- stack overflows in the pass in some symbol visitation paths
- backend failures such as missing procedure codegen or wrong call targets

Goal: keep existing function signatures unchanged (no explicit closure/context
parameters) while making recursive nested-procedure calls correct.


## Requirements

1. Do not change user-visible Fortran signatures.
2. Support recursive host procedures with captured dummy procedures.
3. Keep closure state correct per recursion level.
4. Keep direct recursive host calls with procedure arguments correct.
5. Never infinite-recurse in the pass; fail with explicit diagnostics when
   invariants are broken.
6. Preserve previously working nested tests while fixing `b.f90`.


## High-Level Design

The design keeps the existing 3-phase nested-vars pass architecture and adds a
closure frame stack model in ASR-level transformation:

1. `NestedVarVisitor`:
   - discovers captured symbols
   - prevents visitor recursion loops
   - captures procedure dummy args when internal procedures are present

2. `ReplaceNestedVisitor`:
   - builds per-host synthetic context modules
   - creates closure stack globals and procedure-context globals
   - robustly rewrites captured references and call symbols

3. `AssignNestedVars`:
   - injects frame push/pop and sync logic around calls
   - remaps direct recursive host-call procedure contexts
   - synchronizes scalar/array/procedure captures with explicit policies

Backend logic remains minimal and implements only required runtime behavior for
procedure-variable calls that consume ASR-generated context metadata.

## Trampoline vs Current Model

This implementation does **not** use a trampoline/stub closure ABI.

- The procedure variable still stores a raw function pointer (for example,
  `x4` stores pointer to `one`, `minus_one`, `B`, etc.).
- The closure environment binding is stored separately in an integer context id
  variable `<proc>__ctx` (for example, `x4__ctx`).
- Captured values and captured procedure contexts are stored in module-level
  stack arrays (`<captured>__stack`, `<captured>__ctx__stack`) indexed by the
  context id.

So the effective callable object is represented as:

- pointer: `<proc>`
- environment id: `<proc>__ctx`

not as a synthesized trampoline function pointer.

### How Indirect Calls Get the Right Environment

At ASR level, the pass guarantees that procedure capture assignment updates both
the pointer and its context id:

- pointer sync: `<proc> = <source_proc>`
- context sync: `<proc>__ctx = <source_ctx or current stack ptr>`

At LLVM lowering of indirect procedure calls:

1. Load `saved_stack_ptr` from `__lfortran_nested_ctx_stack_ptr`.
2. Load `ctx_val` from `<proc>__ctx`.
3. Compute `effective_ctx = (ctx_val != 0 && ctx_val != saved_stack_ptr) ? ctx_val : saved_stack_ptr`.
4. Temporarily set `__lfortran_nested_ctx_stack_ptr = effective_ctx`.
5. Materialize base captured globals from `__stack[effective_ctx]`.
6. Perform the indirect call through the raw pointer.
7. Sync selected values back to the active stack slot.
8. Restore `__lfortran_nested_ctx_stack_ptr = saved_stack_ptr` and restore caller view.

This is why the pointer itself does not need to encode environment identity.

### Procedure Pointer/Context Coherency Invariant

For each captured procedure value `p`, correctness requires `p` and `p__ctx` to
be treated as one logical pair.

The pass keeps this pair coherent in all transformed paths that can rebind
captures:

- capture sync generation assigns pointer and context together
  (`AssignNestedVars`, procedure branch),
- direct recursive host-call remap propagates both actual pointer and actual
  context to formals (`remap_direct_host_call_proc_context()`),
- frame push/pop saves/restores both `<proc>__stack` and `<proc>__ctx__stack`.

So if `x4` is changed through a transformed path, `x4__ctx` is updated in the
same lowering step.

If there were an unhandled path that mutates `x4` without corresponding
`x4__ctx` update, that would be a bug (the backend would still call `x4` with
its stale context id). The current implementation relies on pass transformation
coverage of legal rebinding paths.


## ASR-Level Design Details

### 1. Nested symbol discovery (`NestedVarVisitor`)

Key additions:

- `active_functions` set to prevent infinite recursion in `visit_Function`.
- capture of procedure dummy args for functions that own internal procedures.
  This ensures procedure arguments (interface or procedure variable) are
  treated as closure captures when needed.
- in nested `visit_Var`, non-variable symbols are still captured rather than
  recursively descended into.

Result: pass traversal is finite and capture sets include procedure captures
required for recursive closure correctness.


### 2. Synthetic context module shape (`ReplaceNestedVisitor`)

For each host symbol in `nesting_map`, create one synthetic module:

- `__lfortran_nested_ctx_stack_ptr` (i32): active frame id
- `__lfortran_nested_ctx_next_id` (i32): monotonic allocator for new frames
- one global per captured symbol (existing behavior)
- optional `<captured>__stack` array for symbols that require frame slots
- for captured procedures: `<captured>__ctx` (i32) and `<captured>__ctx__stack`

`NESTED_CONTEXT_STACK_SIZE` bounds stack arrays.

Important matching/rewriting robustness:

- `resolve_captured_symbol_key()` and downstream capture-key logic match by:
  symbol identity, past-external identity, declaration symbol, original name,
  and canonicalized dotted suffix-free names.
- call symbol rewrite now reuses existing equivalent `ExternalSymbol` when
  possible, otherwise creates unique import names to avoid collisions.


### 3. Statement transformation and frame protocol (`AssignNestedVars`)

#### 3.1 Frame push/pop protocol

Around call sites that need closure sync:

1. Resolve effective `capture_owner`.
2. Build backup pairs for captured globals and procedure context globals.
3. Allocate a frame id:
   - save current `stack_ptr` in temp
   - use `next_id` when present (monotonic id allocation)
   - bounds-check with `Assert`
4. Copy captured globals to `__stack[frame]`.
5. Set `stack_ptr = frame`.
6. Emit call (plus direct host-call remap logic as needed).
7. Restore globals from stack/current frame as required.
8. Restore `stack_ptr` from saved temp (or decrement fallback).

This prevents recursive levels from aliasing the same closure slot.

#### 3.2 Direct recursive host-call procedure remap

`remap_direct_host_call_proc_context()`:

- detects direct call to capture owner from nested children
- identifies procedure formals among host formals
- remaps formal `<proc>` and `<proc>__ctx` globals from actuals
- for implementation procedures in host scope, uses current frame context
  expression
- emits strict compile-time errors for partial/inconsistent remap when strict
  mode is active

Pointer value remap is intentionally disabled (`remap_ptr_values=false`) in the
direct recursive path to avoid argument-chain corruption; context remap remains.

#### 3.3 Sync policy by capture kind

- Plain scalar captures:
  - normal assign sync
  - additional writeback into current frame slot for closure consistency
- Procedure captures:
  - sync pointer and its `__ctx`
  - nested child scopes route through parent context globals
- Arrays/pointer-like captures:
  - use non-destructive `Associate`-based sync in this path
  - avoid move-style descriptor transfer that can null active locals

#### 3.4 Condition-call sync fixes

For loops/ifs where condition expressions call nested procedures:

- inject pre-body and post-body sync where needed
- for `If`, sync is injected both before and after each arm
- for loops, sync handling includes cycle/condition pathways

This prevents stale or lost closure state when condition evaluation mutates
captured state.


### 4. Additional ASR-level support

- namelist captures are mapped into synthetic modules and imported in nested
  scopes as needed.
- helper utilities were added for:
  - external symbol reuse/import
  - stack slot expression generation
  - capture-owner and capture-key resolution

### 5. `nested_24` (Man-or-Boy) concrete binding example

Source test: `integration_tests/nested_24.f90`.

Key source operations:

- `A(k, x1, x2, x3, x4, x5)` has nested `B`.
- `B` executes `A(m, B, x1, x2, x3, x4)`.
- Base case in `A` executes `x4() + x5()`.

Lowered model for host `A` creates:

- procedure globals: `x1..x5`, `b`
- context globals: `x1__ctx..x5__ctx`, `b__ctx`
- stack arrays for both pointer values and ctx values
- global `__lfortran_nested_ctx_stack_ptr` and `__lfortran_nested_ctx_next_id`

Critical part for recursion step (`B -> A(m, B, x1, x2, x3, x4)`):

1. Current frame is pushed to stack.
2. Pointer actuals are passed normally to `A` (`B`, `x1`, `x2`, `x3`, `x4`).
3. Direct-host-call remap updates context chain before call:
   - `x1__ctx <- b__ctx`
   - `x2__ctx <- old x1__ctx`
   - `x3__ctx <- old x2__ctx`
   - `x4__ctx <- old x3__ctx`
   - `x5__ctx <- old x4__ctx`
4. Call executes.
5. Caller frame/context is restored.

Then at base case `x4()` / `x5()`:

- LLVM call path reads `x4__ctx` / `x5__ctx`,
- switches stack pointer to that context,
- loads that frame's captured values,
- calls raw pointer (`call i32 %fnptr()`),
- restores caller context.

That is the full closure binding path without trampolines.


## Backend Requirements

The ASR design needs a minimal LLVM backend contract for procedure variables.

### 1. LLVM type utility support (`llvm_utils.cpp`)

Add `FunctionType` handling in `get_type_from_ttype_t_util()` so closure stack
storage and loads for procedure values have valid LLVM pointer types.


### 2. Call argument lowering (`asr_to_llvm.cpp`)

In call argument conversion:

- detect procedure values via pointee type and/or `m_type_declaration`
- load pointer level only when pass-by-value is required and safe
- keep intent-aware behavior for inout/out dummy procedures


### 3. Procedure-variable call context switching (`asr_to_llvm.cpp`)

For indirect calls through procedure variables:

1. Locate `<proc>__ctx` and `__lfortran_nested_ctx_stack_ptr`.
2. Compute `effective_ctx`:
   - if ctx is non-zero and differs from saved stack ptr, switch to ctx
   - else keep current stack ptr
3. Materialize capture globals from corresponding `__stack` slot.
4. Null-check function pointer and issue runtime error on null.
5. Perform call.
6. Sync selected values back to stack slot.
7. Restore prior stack ptr and restore base values for caller context.

Selective sync-back policy:

- plain scalar captures skip post-call stack sync-back in this backend path
- non-plain or procedure-related captures sync back

Additional guard:

- for interface-declared procedure variables, avoid unnecessary function-pointer
  reload from storage (`reload_fn_from_storage=false`) to preserve correct call
  target.


## Diagnostics and Safety

Implemented safety behavior includes:

- finite pass traversal via `active_functions`
- explicit remap diagnostics for direct recursive host-call mismatches
- stack-capacity assert in ASR-generated push logic
- runtime null procedure-pointer diagnostic before indirect call


## Behavioral Result

The Man-or-Boy recursion/capture case is correct:

- `b.f90` returns `-67` (same as gfortran)

Coverage added:

- `integration_tests/nested_24.f90` (Man-or-Boy style recursive capture)
- registered in `integration_tests/CMakeLists.txt` with `gfortran` and `llvm`
  labels


## Implementation Checklist (Re-implementation Guide)

If reimplementing from scratch, implement in this order:

1. Add traversal recursion guard and procedure-dummy capture in
   `NestedVarVisitor`.
2. Extend synthetic context module creation with:
   - stack ptr, next id
   - capture stacks
   - procedure context globals and stacks
3. Implement robust capture-key resolution across externals/original names.
4. Add call-symbol rewrite that reuses/imports externals safely.
5. Implement frame push/pop protocol in `AssignNestedVars` around call sites.
6. Implement direct recursive host-call procedure context remap.
7. Add scalar writeback and condition-call sync handling (`While`/`If` paths).
8. Keep array/allocatable sync non-destructive in this path.
9. Add LLVM `FunctionType` type lowering support.
10. Add indirect procedure-call context switch + restore in LLVM codegen.
11. Add runtime null-pointer check for procedure-variable calls.
12. Add/enable Man-or-Boy integration test.


## Known Limitations / Follow-ups

1. Single-threaded global context model: no thread-local isolation yet.
2. Fixed stack capacity (`NESTED_CONTEXT_STACK_SIZE`), currently asserted.
3. Some context replay still exists in LLVM backend; longer-term cleanup is to
   move more semantics into ASR transformation and keep backend more mechanical.
4. Internal procedure lifetime escape handling is not fully modeled as a
   first-class runtime lifetime check yet.
