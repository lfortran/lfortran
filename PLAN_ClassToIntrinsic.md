# Implementation Plan: ClassToIntrinsic Cast (Phase 2)

## Branch: `cast1`

## Design Decision

**(C): Stop `change_symtab` from rewriting intrinsic types.**

Variables inside `type is (...)` blocks stay as `class(*)` in the ASR.
Every use of the variable is wrapped in a `ClassToIntrinsic` Cast node.
On the LHS of an assignment, the Cast is a "store-through": the LLVM
backend extracts the data pointer from the polymorphic wrapper and stores
into it, rather than loading a value out.

---

## Current State (what already works)

| Item | Status |
|------|--------|
| `ClassToIntrinsic` in `ASR.asdl` `cast_kind` enum | ✅ committed |
| `SelectTypeCastInfo` struct (`cast_kind`, `target_type`, `struct_sym`) | ✅ committed |
| `resolve_variable()` wraps `Var` in `Cast` via `select_type_casts_map` | ✅ committed |
| `resolve_variable2()` wraps `Var` in `Cast` for struct-member access | ✅ committed |
| `visit_Cast` → `ClassToIntrinsic` handler (scalar + array) | ✅ committed (dead code) |
| `convert_call_args` → `ClassToIntrinsic` branch (ptr_loads=0) | ✅ committed (dead code) |
| **ClassToIntrinsic emission in semantic analysis** | ❌ all removed |
| **`load_unlimited_polymorphic_value` macro still used (15 sites)** | ❌ still live |
| **`select_type_context_map` still populated and used** | ❌ still live |

All 2433 integration tests pass on the old path.

---

## Overview of Changes

### 1. Semantic Analysis (`ast_body_visitor.cpp`)

#### 1a. `TypeStmtType` – Stop rewriting `assoc_variable->m_type`

**File:** `src/lfortran/semantics/ast_body_visitor.cpp`, lines ~3510-3612

Currently, the `TypeStmtType` handler does:

```cpp
assoc_variable->m_type = view_type;   // or = selector_type
```

This changes the variable from `class(*)` to the guard type (e.g., `integer(4)`).
With design (C), **we must no longer do this**.  The variable's ASR type should
remain as the polymorphic class(*) type.

**What to do:**

- Keep `selector_type` computed (it's needed for the Cast target type and for
  `select_type_casts_map`), keep `view_type` computed, but do NOT assign
  them to `assoc_variable->m_type`.
- `assoc_variable->m_type` should keep whatever it was — the original
  `class(*)` polymorphic type from the selector.  This is already
  `selector_variable->m_type` or `selector_variable_type` depending on
  codepath.  In the `ClassStmt` and `ClassDefault` cases, the code already
  preserves a class-flavored type, so follow the same pattern here.
- Keep `assoc_variable->m_type_declaration` set as before (it may be
  `nullptr` for intrinsic types, which is fine).
- Keep dependencies computed from the guard type since they affect symbol
  resolution ordering.

**Risk:** Other semantic checks may rely on the associate variable being
typed as the guard type. For example, `check_equal_type` in assignment
validation. This may need adjustments downstream: any check that
expects the associate variable to be `integer(4)` will now see `class(*)`.
The Cast node wrapping every use should generally satisfy type checks,
because `expr_type(Cast)` returns `m_type` which IS the guard type.

#### 1b. `TypeStmtType` – Emit `ClassToIntrinsic` in `select_type_casts_map`

**What to do:**

After computing `selector_type` (the guard type) and `view_type`, add an
entry to `select_type_casts_map` for the selector symbol (the `!assoc_sym`
path) or the assoc symbol (the `assoc_sym` path).

For the **`!assoc_sym` path** (no associate name, selector used directly):

```cpp
if (!assoc_sym && ASR::is_a<ASR::Var_t>(*m_selector)) {
    selector_sym_for_map_tst = ASR::down_cast<ASR::Var_t>(m_selector)->m_v;
    // struct_sym is nullptr for intrinsic types (no struct)
    select_type_casts_map[selector_sym_for_map_tst] = {
        ASR::cast_kindType::ClassToIntrinsic,
        view_type,    // or selector_type, depending on array wrapping
        nullptr       // no struct sym for intrinsic types
    };
}
```

For the **`assoc_sym` path** (has an associate name):

```cpp
if (assoc_sym) {
    select_type_casts_map[assoc_sym] = {
        ASR::cast_kindType::ClassToIntrinsic,
        view_type,
        nullptr
    };
}
```

This causes `resolve_variable()` (lines 2243-2252 in `ast_common_visitor.h`)
to wrap every reference to the variable in a `Cast(Var(...), ClassToIntrinsic,
target_type, nullptr, nullptr)`.

**Template:** Follow the same pattern as `TypeStmtName` (ClassToStruct) at
lines 3487-3498.  The only difference is `cast_kind = ClassToIntrinsic` and
`struct_sym = nullptr`.

#### 1c. `TypeStmtType` – Do NOT wrap the `Associate` statement value

In the `assoc_sym` path, we currently create:

```cpp
Associate(dest=Var(assoc_sym), value=m_selector)
```

We must NOT wrap `m_selector` in `ClassToIntrinsic` for the Associate itself,
because we want to associate the polymorphic wrapper pointer, not the extracted
intrinsic value.  The `select_type_casts_map` takes care of wrapping on use.

This is already the current behavior (the ClassToIntrinsic wrapping at
Associate level was already removed).  Just keep it this way.

#### 1d. Remove `current_select_type_guard` tracking

Currently there's a `current_select_type_guard` member that tracks the guard
type during `transform_stmts`.  If we have the `select_type_casts_map` entry
active, this may be redundant.  Investigate whether it's still needed for any
validation.  If not, remove it to simplify code.

---

### 2. LLVM Backend – `visit_Cast` for `ClassToIntrinsic` (Read Path)

**File:** `src/libasr/codegen/asr_to_llvm.cpp`, lines ~12968-13010

The existing handler already does the right thing for the **read path**:

- Scalar: `visit_expr_load_wrapper(arg, 0)` → GEP to field 1 → load i8* →
  bitcast to target type* → load or return pointer based on `ptr_loads`.
- Array: delegates to `convert_class_to_type()`.

**What needs fixing:**

The arg type will now always be unlimited_polymorphic (since we no longer
rewrite the ASR type), so `ASRUtils::expr_type(x.m_arg)` will return
`class(*)`.  The handler already uses the arg type to get the poly_llvm_type,
which is the generic struct `{vptr_type, i8*}`.  This should work correctly.

Verify that `LLVM::is_llvm_pointer(*arg_type)` returns the right value for:
- Non-allocatable `class(*)`: should be false (it's a plain struct alloca).
- Allocatable `class(*)`: should be true (extra pointer indirection).
- Pointer `class(*)`: should be true.

Test this against the failing test cases from Phase 1 (`class_55`, `class_71`,
`class_82` which use allocatable class(*)).

---

### 3. LLVM Backend – Store-Through on LHS (Write Path)

**The hard part.** When `ClassToIntrinsic` appears as `x.m_target` in an
`Assignment_t`, we need to:

1. Evaluate the RHS to get an intrinsic LLVM value.
2. Evaluate the LHS's inner `Var` (the polymorphic variable) to get a
   pointer to the polymorphic wrapper.
3. GEP to field 1 (data pointer), load i8*, bitcast to target-type*,
   and **store** the RHS value into it.

#### 3a. Detect `ClassToIntrinsic` on the target

In `visit_Assignment`, check if `x.m_target` is a `Cast_t` with
`kind == ClassToIntrinsic`.

```cpp
if (ASR::is_a<ASR::Cast_t>(*x.m_target) &&
    ASR::down_cast<ASR::Cast_t>(x.m_target)->m_kind ==
        ASR::cast_kindType::ClassToIntrinsic) {
    // Handle store-through
    ASR::Cast_t* cast = ASR::down_cast<ASR::Cast_t>(x.m_target);
    handle_class_to_intrinsic_store(cast, x.m_value);
    return;
}
```

Add this check **early** in `visit_Assignment`, before the target type
dispatch at line ~8503.  Place it right after the `x.m_overloaded` check
(line ~8494).

#### 3b. Implement `handle_class_to_intrinsic_store`

```cpp
void handle_class_to_intrinsic_store(ASR::Cast_t* target_cast,
                                     ASR::expr_t* rhs) {
    ASR::ttype_t* target_type = target_cast->m_type;   // e.g., integer(4)
    ASR::ttype_t* poly_type = ASRUtils::expr_type(target_cast->m_arg); // class(*)

    // 1. Evaluate RHS
    int64_t ptr_loads_copy = ptr_loads;
    ptr_loads = 2;  // load to value
    this->visit_expr_wrapper(rhs, true);
    llvm::Value* rhs_value = tmp;

    // 2. Evaluate LHS (the class(*) variable — NOT the Cast)
    ptr_loads = 0;  // get pointer to polymorphic wrapper
    this->visit_expr(*target_cast->m_arg);
    llvm::Value* poly_ptr = tmp;
    ptr_loads = ptr_loads_copy;

    // 3. Handle allocatable/pointer extra indirection
    llvm::Type* poly_llvm_type = llvm_utils->get_type_from_ttype_t_util(
        target_cast->m_arg, ASRUtils::extract_type(poly_type), module.get());
    if (LLVM::is_llvm_pointer(*poly_type)) {
        poly_ptr = llvm_utils->CreateLoad2(poly_llvm_type->getPointerTo(), poly_ptr);
    }

    // 4. GEP to data field (index 1), load i8*, bitcast, store
    llvm::Type* target_llvm_type = llvm_utils->get_type_from_ttype_t_util(
        const_cast<ASR::expr_t*>(&target_cast->base),
        ASRUtils::extract_type(target_type), module.get());
    llvm::Value* data_ptr = llvm_utils->create_gep2(poly_llvm_type, poly_ptr, 1);
    data_ptr = llvm_utils->CreateLoad2(llvm_utils->i8_ptr, data_ptr);
    data_ptr = builder->CreateBitCast(data_ptr, target_llvm_type->getPointerTo());
    builder->CreateStore(rhs_value, data_ptr);
}
```

**Special cases to handle:**

- **String assignment:** If target_type is `String`, need to use
  `lfortran_str_copy` instead of a raw store.  Check `is_character`.
- **Array assignment:** If the guard type is an array of intrinsic type,
  the store-through must adjust array descriptors (similar to
  `convert_class_to_type` for arrays but in reverse).  Initially, we may
  defer array LHS to the existing `load_unlimited_polymorphic_value` path
  and only handle scalar store-through.  But if `select_type_context_map`
  is to be removed, arrays need full support.

---

### 4. LLVM Backend – Removing `load_unlimited_polymorphic_value` Macro

**File:** `src/libasr/codegen/asr_to_llvm.cpp`, lines 420-438

The macro `load_unlimited_polymorpic_value` (note typo in name) checks at
each of its 15 call sites whether a `Var_t` expression is unlimited
polymorphic, and if so, looks up `select_type_context_map` to extract the
underlying intrinsic value.

With `ClassToIntrinsic`, the variable reference itself will already be
wrapped in a Cast node, so `visit_Cast(ClassToIntrinsic)` handles the
extraction.  The macro call sites will no longer see a bare `Var_t` for the
class(*) variable—they'll see a `Cast_t` node, and visiting it will already
produce the extracted intrinsic value.

**What to do:**

After ClassToIntrinsic is fully functional:

1. **Verify** that each of the 15 call sites no longer gets a bare
   unlimited-polymorphic Var.  They should all receive Cast-wrapped
   expressions.  Add `LCOMPILERS_ASSERT` if needed to confirm.
2. Remove the macro body (make it a no-op).
3. Run all tests.
4. If tests pass, delete the macro and all 15 call sites entirely.

**Call sites to clean up:**

| Line   | Context                    |
|--------|----------------------------|
| 9104   | `visit_Assignment` — value side (general path, after struct/class/poly branches) |
| 10496  | `visit_IntegerCompare` — left |
| 10497  | `visit_IntegerCompare` — right |
| 10539  | `visit_RealCompare` — left |
| 10540  | `visit_RealCompare` — right |
| 10625  | `visit_ComplexCompare` — left |
| 10626  | `visit_ComplexCompare` — right |
| 10978  | `visit_StringCompare` — left |
| 10979  | `visit_StringCompare` — right |
| 11076  | `visit_StringLen` — arg |
| 11320  | `visit_IntegerBinOp` — left |
| 11321  | `visit_IntegerBinOp` — right |
| 11449  | `visit_RealBinOp` — left |
| 11450  | `visit_RealBinOp` — right |
| 12475  | `visit_IntegerUnaryMinus` — arg |

---

### 5. LLVM Backend – Removing `select_type_context_map`

**File:** `src/libasr/codegen/asr_to_llvm.cpp`

The `select_type_context_map` maps from a symbol (selector or assoc) to
`SelectTypeContext { block_type, block_type_asr, der_type, selector_type_decl }`.

It is used by:
1. `load_unlimited_polymorpic_value` (via `get_select_type_block_type_asr()`) —
   covered above.
2. `visit_SelectType` itself to set up the context for each guard block.
3. `convert_class_to_type()` may use it indirectly (it doesn't appear to
   directly reference it, but `change_symtab` does).

**What to do:**

After step 4 is complete:
1. Remove population of `select_type_context_map` in `visit_SelectType`
   (lines ~10146, ~10450-10467).
2. Remove the `SelectTypeContext` struct (line 252) and
   `select_type_context_map` declaration (line 259).
3. Remove helper methods `get_select_type_block_type()`,
   `get_select_type_block_type_asr()` (lines 261-290).
4. Run tests.

**Note:** `TypeStmtName` (ClassToStruct) and `ClassStmt` (ClassToClass) still
use `ctx.block_type` in their LLVM codegen for the type-comparison branches.
This is used for the **type-checking condition** (comparing vptr/typeinfo),
not for value extraction.  The condition needs `ctx.block_type` to know what
type to compare against, but this is set and used **within the same
visit_SelectType iteration**, not across scopes.  So we can keep
`ctx.block_type` as a local variable within the switch-case rather than a map
entry.

Actually, `change_symtab` (line 10429) does use
`ctx.block_type` to bitcast the selector's data.  But `change_symtab` is
already gated on `!compiler_options.new_classes`, so for the `new_classes=true`
path, it's unused.  We can keep `select_type_context_map` alive only for the
`!new_classes` path if we want backward compatibility, or eventually remove
both paths.

**Recommendation:** Initially, keep `select_type_context_map` alive and just
make the macro a no-op.  This ensures backward compatibility with the
`!new_classes` path.  In a later cleanup pass, remove the map entirely when
`!new_classes` is dropped.

---

### 6. LLVM Backend – `convert_call_args` for ClassToIntrinsic

**File:** `src/libasr/codegen/asr_to_llvm.cpp`, lines ~16536-16540

The existing branch at line ~16536 already handles `ClassToIntrinsic` in
call arguments:

```cpp
} else if (ASR::is_a<ASR::Cast_t>(*x.m_args[i].m_value) &&
            ASR::down_cast<ASR::Cast_t>(x.m_args[i].m_value)->m_kind ==
                ASR::cast_kindType::ClassToIntrinsic) {
    this->visit_expr_load_wrapper(x.m_args[i].m_value, 0);
}
```

This visits the Cast with `ptr_loads=0`, which means the ClassToIntrinsic
handler returns a **pointer** to the intrinsic value, not the value itself.
This is correct for pass-by-reference (intent(in), intent(inout), intent(out)).

**What needs checking:**

- For `intent(in)` with `value` attribute (pass-by-value), an extra load
  may be needed.  Check if this occurs in tests.
- For array arguments, the conversion should produce a descriptor with
  the correct element type. The `convert_class_to_type` array path
  already handles this, and `ClassToIntrinsic` delegates to it for arrays.

---

### 7. LLVM Backend – `visit_Assignment` Unlimited Polymorphic Branch

**File:** `src/libasr/codegen/asr_to_llvm.cpp`, lines ~8828-8865

Currently, `visit_Assignment` has a branch:
```cpp
} else if (compiler_options.new_classes &&
            (is_value_unlimited_polymorphic || is_target_unlimited_polymorphic)) {
```

With design (C), the target will no longer be `is_target_unlimited_polymorphic`
when it's wrapped in a `ClassToIntrinsic` Cast (because `expr_type(Cast)` returns
the intrinsic type, not class(*)).  So this branch will NOT fire for the
store-through case.  Instead, our new early check (step 3a) handles it.

**However**, for the case where a value is class(*) being assigned TO an
intrinsic (not inside select type), this branch should still work.  Verify.

Also: for `is_value_unlimited_polymorphic = true, is_target_unlimited_polymorphic
= true` (class(*) to class(*)), this branch handles deepcopy.  This should still
work since neither side is wrapped in ClassToIntrinsic.

---

### 8. LLVM Backend – `visit_SelectType` Cleanup

**File:** `src/libasr/codegen/asr_to_llvm.cpp`, lines ~10121-10510

In the `TypeStmtType` case, `visit_SelectType` currently sets:
```cpp
ctx.block_type = llvm_utils->get_type_from_ttype_t_util(...);
ctx.block_type_asr = type_stmt_type;
```

And later adds the assoc symbol to `select_type_context_map`.

With ClassToIntrinsic, the value extraction is handled by the Cast node, so:
- `ctx.block_type_asr` for `TypeStmtType` is only consumed by
  `load_unlimited_polymorphic_value`.  Once that macro is removed, this
  can be skipped.
- The assoc symbol mapping (lines ~10456-10466) can be removed.
- `change_symtab` (lines 10429-10463) is already gated on `!new_classes`.

**What to do:**
- For the `new_classes` path, skip populating `select_type_context_map`
  for `TypeStmtType` blocks.
- Keep it populated for `TypeStmtName` and `ClassStmt` unless those are
  also converted to use Cast nodes for all value extraction (they already
  use ClassToStruct/ClassToClass, but the macro still fires for some
  edge cases).

---

## Implementation Order

### Phase 2a: Enable ClassToIntrinsic Emission + Store-Through (Minimum Viable)

1. ✅ **Emit ClassToIntrinsic** in `select_type_casts_map` for both
   `assoc_sym` and `!assoc_sym` paths in `TypeStmtType` (scalars only).
2. ✅ **Stop rewriting** `assoc_variable->m_type` — keep it as `class(*)`
   for scalars. Arrays still rewrite to the guard type.
3. ✅ **Add store-through** in `visit_Assignment` for `ClassToIntrinsic`
   on LHS (including string special case via `lfortran_str_copy`).
4. ✅ **`visit_Cast` handler** for `ClassToIntrinsic` — read path for
   both scalar (GEP + bitcast + load) and array (delegates to
   `convert_class_to_type`).
5. ✅ **`convert_call_args`** handles `ClassToIntrinsic` with `ptr_loads=0`
   for pass-by-reference.
6. ✅ **LHS validity check** updated to allow `ClassToIntrinsic` Cast as
   assignment target.
7. ✅ **`SelectTypeCastInfo`** refactored: `bool is_class_is` replaced
   with `ASR::cast_kindType cast_kind` enum.
8. ✅ **`resolve_variable()`** handles nullable `struct_sym` for
   `ClassToIntrinsic` (no dest expr needed).
9. ✅ **String-as-call-argument fix:** store to temp alloca when
   value is not a pointer (e.g., from `ClassToIntrinsic` cast).
10. ✅ **New test `select_type_27`** — `real()`/`aimag()` intrinsics on
    `class(*)` complex values.
11. ✅ **All integration tests pass.**

### Phase 2b: Clean Up Old Mechanisms

6. ✅ Make `load_unlimited_polymorpic_value` macro a **no-op** — confirmed
   dead code: all 15 call sites are never reached because expressions are
   Cast_t nodes (not bare Var_t) or variable types are rewritten.
7. ✅ Test — zero new failures with macro as no-op.
8. ✅ Delete macro definition and all 15 call sites. Also removed the dead
   polymorphic string special case in `visit_StringLen`.
9. ✅ Stop populating `select_type_context_map` for `TypeStmtType` under
   `new_classes`: skip setting `ctx.block_type_asr` and skip adding
   assoc_sym to the map. TypeStmtName/ClassStmt entries preserved.
10. `SelectTypeContext` struct and helper methods kept — still needed for
    TypeStmtName/ClassStmt context map entries and the `!new_classes`
    legacy path. Can be removed when those are also migrated.
11. ✅ All integration tests pass.

### Phase 2b (Investigate)

12. **`current_select_type_guard`** — still used at line ~3656-3659
    (set/restore around `transform_stmts`) and consumed at line ~6691
    for string validation. Investigate whether it can be removed or if
    it's still needed for that check.

### Phase 2c: Unification (Optional) — TODO

13. Extend `ClassToIntrinsic` to **arrays**, eliminating the fallback to
    the old mechanism (rewriting `assoc_variable->m_type` +
    `select_type_context_map` + `load_unlimited_polymorpic_value`).
    This is the main blocker for fully removing the old path.
14. Consider converting `TypeStmtName` to also use `select_type_casts_map`
    for all value extraction (it already does for `!assoc_sym`).
15. Consider removing `select_type_context_map` entirely (for `new_classes`).
16. Verify `resolve_variable2` and struct member access: no test should do
    `x%something` inside a `type is (integer)` block (ClassToIntrinsic
    variables are not structs).
17. Remove remaining dead code paths that check context map for
    unlimited-polymorphic types (e.g., `extract_kinds`, `visit_Assignment`
    polymorphic array branch, `SerializeType` context lookup, StringToArray
    polymorphic branch). These are unreachable under `new_classes` since
    all select type variables are now Cast-wrapped or type-rewritten, but
    kept for the legacy `!new_classes` path.

---

## Test Cases to Watch

| Test | What it exercises |
|------|-------------------|
| `select_type_01` – `select_type_10` | Basic select type with intrinsic types |
| `class_55` | Allocatable `class(*)`, integer guard |
| `class_71` | Allocatable `class(*)`, assign inside guard |
| `class_82` | Allocatable `class(*)` array |
| `procedure_28` | Class(*) as procedure argument |
| `separate_compilation_class_star_01` | Class(*) with separate compilation, assignment on LHS |
| `select_rank_10`, `select_rank_13` | Select rank + select type combined |
| `class_08` | Class hierarchy with select type |
| `class_star_tests/*` | Julienne test suite for class(*) operations |

---

## Known Risks

1. **ASR type check mismatch:** With `assoc_variable->m_type` staying as
   `class(*)`, some semantic checks that compare variable types may fail.
   The Cast node's `m_type` provides the expected type, but checks that
   look at the Variable directly (not through the expression) will see
   `class(*)`.  May need to relax or adjust those checks.

2. **String store-through:** Character types need `lfortran_str_copy`
   rather than a simple LLVM store.  The store-through handler must
   detect character types and dispatch correctly.

3. **Array store-through:** Assigning to a class(*) array element inside
   a `type is` block produces an `ArrayItem` of a `Cast`.  The LLVM
   backend must handle `ArrayItem(Cast(Var, ClassToIntrinsic, ...))`.
   This may require the `ArrayItem` handler to recognize ClassToIntrinsic
   on its array argument and extract the data pointer before indexing.

4. **`resolve_variable2` and struct member access:** The `resolve_variable2`
   function also checks `select_type_casts_map`, but for ClassToIntrinsic
   the variable is not a struct, so member access shouldn't occur.  Verify
   that no test tries `x%something` inside a `type is (integer)` block.

5. **Nested select type:** If a select type block is nested inside another,
   the `select_type_casts_map` must be properly scoped.  The current
   save/restore pattern (set before `transform_stmts`, erase after) handles
   this correctly.

6. **`ClassDefault` blocks:** These keep the variable as class(*) and do NOT
   add any Cast.  No change needed for ClassDefault.

---

## Files to Modify

| File | Changes |
|------|---------|
| `src/lfortran/semantics/ast_body_visitor.cpp` | Emit ClassToIntrinsic in TypeStmtType, stop rewriting m_type |
| `src/libasr/codegen/asr_to_llvm.cpp` | Add store-through in visit_Assignment; later: remove macro + map |
| Tests | No new test files needed; all changes validated by existing suite |
