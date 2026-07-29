# Review Examples

These compact examples illustrate recurring implementation patterns. They are
templates for reasoning, not project history or line-level prescriptions.

## Contents

- Backend type inference and explicit error flow
- Explicit options, normalized type paths, and shared pass machinery
- Record semantics and proportional tests
- Located diagnostics, narrow layering, and end-to-end tests
- Explicit ASR layout guarantees and portability warnings
- Canonical and idempotent symbol handling
- Explicit provenance and discriminant-aware fields
- Syntax normalization in AST-to-ASR
- Semantic diagnostics after parsing and scope-bearing Blocks
- Native interoperability headers and runtime buffer capacities
- Real assertions and append-only diagnostic fixtures
- Lexer/grammar composition and version-independent helpers
- Shared type-size logic
- Root-cause discipline for external CI

## Bad: infer an ASR fact from LLVM and bitcast

```diff
+llvm::Type *expected_type = llvm_utils->get_type_from_ttype_t_util(
+    ASRUtils::EXPR(ASR::make_Var_t(...)), orig_arg->m_type, module.get());
+if (tmp->getType() != expected_type) {
+    tmp = builder->CreateBitCast(tmp, expected_type);
+}
```

**Problem:** LLVM type inequality is being used to discover a semantic
distinction. The cast may hide an incorrect ASR type, physical representation,
or procedure interface.

**Rework:** identify why actual and formal types differ. Resolve the distinction
in semantics or represent the required procedure representation explicitly in
ASR, verify it, and let LLVM lowering emit the already-known target type.

## Bad to good: exception control flow to `Result`

Bad:

```diff
+diagnostics.add(diag::Diagnostic(...));
+throw parser_local::ParserAbort();
...
+} catch (const parser_local::ParserAbort &) {
+    Error error;
+    return error;
+}
```

Good:

```cpp
bool process_include(..., diag::Diagnostics &diagnostics) {
    if (!file_found) {
        diagnostics.add(diag::Diagnostic(...,
            {diag::Label("", {loc})}));
        return false;
    }
    Result<std::string> include_res = prescan(..., diagnostics);
    if (!include_res.ok) return false;
    include = include_res.result;
    return true;
}
```

**Why:** expected compiler failures remain visible in the call chain, and the
diagnostic retains its source location.

## Bad to good: magic-string intent

Bad:

```python
if "--show-fortran" in cmd:
    stdout_ext = ".f90"
else:
    stdout_ext = ".stdout"
```

Good:

```python
def run(..., stdout_ext=".stdout"):
    stdout_file = os.path.join(out_dir, basename + stdout_ext)

run(..., stdout_ext=".f90")
```

**Why:** the caller already knows the output kind. A generic helper should not
parse a command string to rediscover intent.

## Bad to good: duplicate a scalar path for arrays

Bad:

```cpp
if (is_struct(target_type)) {
    // resolve ~assign and walk parents
}
if (!sym && is_array_of_struct(target_type)) {
    // copy of the same resolution and parent walk
}
```

Good:

```cpp
ASR::ttype_t *base_target_type = ASRUtils::extract_type(target_type);
ASR::ttype_t *base_value_type = ASRUtils::extract_type(value_type);
sym = resolve_assignment(base_target_type, base_value_type);
```

**Why:** normalizing once keeps scalar and array behavior aligned and prevents
future fixes from changing only one copied path.

## Bad: rebuild pass machinery locally

```cpp
Vec<ASR::expr_t*> idx_vars;
// create indices, compute bounds, build element references,
// construct nested DoLoop nodes, and replace the statement
```

**Rework:** keep the special case to detection and argument preparation. Reuse
`PassUtils::get_bound`, `get_index_kind()`, `create_array_ref`, and an existing
loop wrapper or `generate_loop`.

## Bad: flatten records because memory is contiguous

```cpp
str_src_data = llvm_utils->get_stringArray_data(array_type, unit_val);
llvm::Value *elem_len =
    llvm_utils->get_stringArray_length(array_type, unit_val);
// Treat elem_len * n_elems as one stream.
```

For `character(2) :: lines(2) = ["42", "43"]`, `read(lines, *) a, b`
must read two records. Flattening exposes `"4243"` as one record and changes
language semantics.

**Rework:** preserve element length and count as separate record metadata,
preferably before backend lowering; make the runtime iterate records. Add this
exact multi-record, no-padding test.

## Bad: a huge fixture for one small behavior

```diff
@@ -0,0 +1,1300 @@
+program runtime_stacktrace_test
```

**Rework:** reduce the source to the smallest program that triggers the same
bounds failure. If compact coverage already exists, do not commit an unrelated
large fixture.

## Good: semantic diagnostics retain source facts

```diff
-std::vector<std::string> assgn_proc_names;
+std::vector<std::pair<std::string, Location>> assgn_proc_names_locations;
...
-ASR::symbol_t *sym = current_scope->resolve_symbol(name);
+ASR::symbol_t *sym =
+    current_scope->resolve_symbol(to_lower(name_loc.first));
...
-{Label("", {})}
+{Label("", {name_loc.second})}
```

**Why:** source location survives semantic processing, symbol lookup follows
Fortran case rules, and failures point to the relevant source.

## Good: choose the narrow correct layer

```cpp
std::string cpp_input = code_orig;
if (!cpp_input.empty() && cpp_input.back() != '\n') {
    cpp_input.push_back('\n');
}
Result<std::string> res =
    cpp.run(cpp_input, lm, cpp.macro_definitions, diagnostics);
```

File-like inputs require a trailing newline, while the shared preprocessor also
handles recursive macro fragments that must not gain one. Normalize at the
file-like entry point, not unconditionally in the deepest shared function.

## Good: compact end-to-end regression

```cmake
RUN(NAME defined_op_match_02 LABELS gfortran llvm)
```

Pair the registration with a small program containing `error stop` checks for
present, absent, whole-array, section, and scalar cases. This makes the test
behavioral, registered, and differential across the reference and LLVM
backends.

## Good: encode a layout guarantee in ASR

```diff
-| Struct(... bool is_packed, bool is_abstract, ...)
+| Struct(... bool is_packed, bool is_abstract, bool is_sequence, ...)
```

Set the field during AST-to-ASR:

```cpp
bool is_sequence = has_attribute(x, AST::simple_attributeType::AttrSequence);
ASR::make_Struct_t(..., is_packed, is_abstract, is_sequence, ...);
```

Consume the semantic fact mechanically:

```cpp
bool has_defined_layout =
    struct_type->m_abi == ASR::abiType::BindC ||
    struct_type->m_is_sequence;
if (has_defined_layout) {
    // Lower fixed character members as inline storage.
}
```

**Why:** the frontend records the language-level layout guarantee once.
Codegen no longer has to infer it from an LLVM type or member shape, and ASR
verification can reject inconsistent states.

## Good: warn when accepting a language extension

```cpp
if (!has_len || char_len != 1) {
    diag.semantic_warning_label(
        "character component of a BIND(C) type with length other than 1 "
        "is not standard-conforming (LFortran extension)",
        {member->base.base.loc},
        "use character(len=1) or an array of length-1 elements");
}
```

Register runtime behavior only for compilers that accept the extension, and add
a reference diagnostic that checks the warning location and conforming
alternative.

**Why:** useful extensions remain available without misleading users about
portability. Testing both behavior and diagnostics prevents the extension from
silently becoming an undocumented language divergence.

## Good: import the terminal symbol definition

Bad:

```cpp
ASR::symbol_t *struct_sym = get_struct_sym_from_struct_expr(expr);
ASR::make_ExternalSymbol_t(..., struct_sym, ...);
```

Good:

```cpp
ASR::symbol_t *struct_sym = get_struct_sym_from_struct_expr(expr);
ASR::symbol_t *original_struct =
    ASRUtils::symbol_get_past_external(struct_sym);
if (original_struct == nullptr) return nullptr;
ASR::make_ExternalSymbol_t(..., original_struct,
    ASRUtils::symbol_name(ASRUtils::get_asr_owner(original_struct)), ...);
```

**Why:** an imported symbol points directly to its definition rather than
forming an `ExternalSymbol` chain. Transitive imports then have one canonical
owner and downstream consumers do not need repeated unwrapping.

## Good: make symbol creation idempotent

Bad:

```cpp
ASR::symbol_t *var_sym = make_index_variable(...);
current_scope->add_symbol(var_name, var_sym);
```

Good:

```cpp
ASR::symbol_t *var_sym = current_scope->get_symbol(var_name);
if (var_sym == nullptr) {
    var_sym = make_index_variable(...);
    current_scope->add_symbol(var_name, var_sym);
}
ASR::expr_t *var = ASRUtils::EXPR(
    ASR::make_Var_t(al, loc, var_sym));
```

**Why:** repeated or overlapping lowering paths reuse the current-scope symbol
instead of creating duplicate names and invalidating symbol-table invariants.
The compatibility of an existing symbol must still be checked when the context
does not guarantee it.

## Bad to good: derive provenance instead of matching generated names

Bad:

```cpp
if (starts_with(symbol_name, "1_") ||
    starts_with(symbol_name, "__compiler_marker_")) {
    entry.hidden = true;
}
```

Good:

```cpp
ASR::symbol_t *definition =
    ASRUtils::symbol_get_past_external(symbol);
ASR::symbol_t *owner =
    definition ? ASRUtils::get_asr_owner(definition) : nullptr;

entry.is_synthetic_member =
    owner && (ASR::is_a<ASR::Struct_t>(*owner) ||
              ASR::is_a<ASR::Enum_t>(*owner) ||
              ASR::is_a<ASR::Union_t>(*owner));
```

If several consumers need this distinction, store the provenance flag in ASR
instead of recomputing it.

**Why:** imported user symbols and synthetic member lookups can share a node
kind. Generated names are implementation details and rarely cover every source
of an entity.

## Bad to good: access kind-dependent storage through one abstraction

Bad:

```cpp
double value = ASR::down_cast<ASR::RealConstant_t>(
    ASRUtils::expr_value(arg))->m_r;
```

If `m_r` stores a floating value for common kinds but encodes a pointer to wider
storage for another kind, the unconditional read silently produces nonsense.

Good shape:

```cpp
RealValue value = ASRUtils::extract_real_constant(arg);
switch (value.kind) {
    case 4:
    case 8:
        use_native(value.native);
        break;
    case 16:
        use_wide(value.wide);
        break;
}
```

Alternatively, use distinct explicit fields whose types make the
representations impossible to confuse.

**Why:** every reader honors the same discriminant. Do not add another field
that merely duplicates the kind already stored in the expression type.

## Good: normalize syntax before backend lowering

```diff
+| KW_TYPE KW_IS "(" TK_NAME "(" kind_arg_list ")" ")" sep statements {
+    $$ = TYPE_STMTVAR(
+        ATTR_TYPE_NAME_KIND(Type, SYMBOL($4, @4), $6, @$),
+        TRIVIA_AFTER($9, @$), $10, @$);
+}
```

In AST-to-ASR, recognize that the parsed type denotes a derived type and lower
it to the same canonical type-guard representation used by the simpler syntax:

```cpp
ASR::symbol_t *type_decl =
    ASRUtils::symbol_get_past_external(parsed_type_declaration);
if (type_decl && ASR::is_a<ASR::Struct_t>(*type_decl)) {
    lower_as_struct_type_guard(type_decl, selector, body);
}
```

**Why:** grammar handles the source form, semantics resolves its meaning, and
the backend receives an existing canonical ASR node.

## Bad to good: semantic validation in a parser action

Bad:

```cpp
#define REQUIREMENT(name, args, body, loc) ( \
    check_duplicate_args(parser, args), \
    make_Requirement_t(...))
```

Good: retain each argument's source location in AST and validate it during
AST-to-ASR:

```cpp
std::map<std::string, Location> seen;
for (const AST::arg_t &arg_node : requirement_args) {
    std::string arg = to_lower(arg_node.m_arg);
    auto previous = seen.find(arg);
    if (previous != seen.end()) {
        diag.add(Diagnostic(
            "parameter '" + arg + "' is declared more than once",
            Level::Error, Stage::Semantic, {
                Label("first declared here", {previous->second}),
                Label("redeclared here", {arg_node.loc})}));
        throw SemanticAbort();
    }
    seen.emplace(arg, arg_node.loc);
}
```

**Why:** parsing always produces AST. Semantics can identify both precise
offending tokens and participate in continue-compilation recovery.

## Bad to good: assert the condition, not the message

Bad:

```cpp
LCOMPILERS_ASSERT("parameter number is in range");
if (i >= call.n_args) continue;
```

The string is always truthy, and the `continue` hides an invalid formal/actual
mapping.

Good:

```cpp
LCOMPILERS_ASSERT(i < call.n_args);
LCOMPILERS_ASSERT(formal_count == actual_count ||
    omitted_arguments_are_valid(call));
```

When the invariant applies to all valid ASR, add the equivalent requirement to
ASR verification rather than relying only on a backend assertion.

## Good: keep a shared diagnostic fixture append-only

Bad:

```fortran
program continue_compilation
    ! Existing cases and internal helper procedures are interleaved.
    ! Inserting a new case shifts every later diagnostic line.
end program
```

Good:

```fortran
module diagnostic_helpers
contains
    ! Stable helper procedures.
end module

program continue_compilation
    use diagnostic_helpers
    ! Existing cases remain fixed; append new cases here.
end program
```

**Why:** source locations in existing reference diagnostics stay stable while
the file remains a single recoverable-error test.

## Bad to good: put local scope on a symbol

Bad ASR shape:

```text
DoConcurrentLoop(symbol_table symtab, do_loop_head* head, stmt* body)
```

A statement node should not own a symbol table.

Good:

```text
DoConcurrentLoop(do_loop_head* head, stmt* body)
Block(symbol_table symtab, identifier name, stmt* body)
```

When the loop header declares local names, lower the body to a `BlockCall`
referencing a compiler-generated `Block`. Require that shape in ASR verification
and visit the loop under the Block's scope.

**Why:** symbol ownership remains uniform, and every consumer follows the same
verified representation instead of learning another statement-specific scope
exception.

## Bad to good: use each compiler's interoperability header

Bad:

```c
#define _lfortran_cfi_calloc calloc
#define _lfortran_cfi_free free
#include "../src/libasr/runtime/ISO_Fortran_binding.h"
```

Good:

```c
#include "ISO_Fortran_binding.h"
```

Configure each compiler invocation so its include path and runtime provide the
matching standard interoperability implementation.

**Why:** a differential test must exercise each compiler's ABI, not compile all
variants against one compiler's private descriptor definitions and shims.

## Bad to good: respect the runtime buffer contract

Bad:

```c
snprintf(iomsg, iomsg_len + 1,
    "cannot open file '%s': %s", filename, reason);
```

If `iomsg` points to `iomsg_len` bytes, the advertised capacity is one byte too
large.

Good:

```c
if (iomsg != NULL && iomsg_len > 0) {
    snprintf(iomsg, (size_t)iomsg_len,
        "cannot open file '%s': %s", filename, reason);
}
```

**Why:** bounded APIs trust the supplied capacity. Tests should exercise every
error branch with short, exact-size, and truncated message buffers.

## Bad to good: extend the grammar at the compositional rule

Bad:

```text
decl_statements : decl_statements special_decl
var_decl_star   : var_decl_star   special_decl
decl_star       : decl_star       special_decl
```

Good:

```text
decl_statement : ordinary_decl
               | special_decl

decl_statements : decl_statements decl_statement
```

Add a parser-level test for every new lexer token and audit all source-form
lexer paths that emit it.

**Why:** one per-item production composes through every existing list context.
Patching each wrapper duplicates grammar knowledge and inevitably misses one.

## Bad to good: isolate toolchain-version compatibility

Bad:

```cpp
#if LLVM_VERSION_MAJOR >= 15
    value = load_new(...);
#else
    value = load_old(...);
#endif
```

repeated at several lowering sites.

Good:

```cpp
value = llvm_utils->load_compatible(pointer, element_type);
```

with the version conditional implemented once inside `load_compatible`.

**Why:** semantic lowering no longer knows toolchain-version details, and
compatibility behavior has one implementation and one focused test surface.

## Bad to good: duplicate size calculations

Bad:

```cpp
auto [size, align] = compute_type_size_align(type);
if (ASRUtils::is_string(type)) {
    size *= extract_string_length(type);
}
```

Repeated copies of the string adjustment will drift and can double-apply the
length.

Good:

```cpp
if (ASR::is_a<ASR::String_t>(*type)) {
    ASR::String_t *string_type = ASR::down_cast<ASR::String_t>(type);
    int64_t len;
    if (string_type->m_kind <= 0 ||
        string_type->m_len == nullptr ||
        !ASRUtils::extract_value(string_type->m_len, len) ||
        len < 0) {
        return {-1, -1};
    }
    return {string_type->m_kind * len, 1};
}
```

**Why:** the shared size-and-alignment helper owns the complete definition.
Callers no longer add type-specific corrections.

## Bad: disable external coverage without finding the failing toolchain

```sh
# Temporary fix
git cherry-pick <upstream-commit-that-disables-this-compiler>
```

**Problem:** the imported patch may reduce compiler coverage while the observed
failure actually comes from a reference compiler, runtime, launcher, or
dependency.

**Rework:** identify the command and toolchain that fails, preserve unaffected
coverage, and fix the root cause. If quarantine is unavoidable, skip only the
specific failing case, attach it to a tracked defect, and add a condition that
can be removed when the defect is fixed.
