# Function

Function is a **symbol** node representing a procedure (function or subroutine).

## Declaration

### Syntax

```
Function(symbol_table symtab, identifier name, ttype function_signature,
    identifier* dependencies, expr* args, stmt* body, expr? return_var,
    access access, bool deterministic, bool side_effect_free,
    string? module_file, string? link_name,
    location start_name, location end_name)
```

### Arguments

`symtab` local symbol table of the procedure (arguments, locals, nested
symbols). Its parent is the table that contains this Function.

`name` the name of the procedure in the symbol table. This is the ASR /
symbol-table identity of the procedure. It may differ from the source spelling
(names are typically lowercased) and may be disambiguated when it would
otherwise clash with another symbol in the same table (see `link_name`).

`function_signature` a `FunctionType` describing argument types, return type,
ABI, deftype (Implementation vs Interface), purity flags, and related type-level
attributes (including `bindc_name` for BindC export names).

`dependencies` names of other symbols this procedure depends on; they must be
resolvable from the parent symbol table.

`args` formal argument expressions (usually `Var` nodes into `symtab`).

`body` executable statements. Empty for interface bodies
(`deftype == Interface`).

`return_var` result variable for functions; null for subroutines.

`access` visibility: `Public` or `Private`.

`deterministic` whether the procedure is side-effect free with respect to
global state in the sense used by the compiler (set by semantics / analysis).

`side_effect_free` whether calls can be treated as free of observable side
effects.

`module_file` optional path / header association used for some external or
imported procedures; null for ordinary procedures.

`link_name` optional **object-file / external linkage name** when it differs
from `name`. Null means backends must use `name` (after normal mangling rules
for the ABI and deftype). See [link_name](#link_name) below.

`start_name` / `end_name` optional source locations of the procedure’s name
token(s) for diagnostics.

### Return values

None (symbol node).

## Description

A `Function` represents both functions and subroutines. Whether a procedure is
a function is indicated by a non-null `return_var` and a non-null
`FunctionType.return_var_type`.

`FunctionType` on `function_signature` carries type- and ABI-level facts:

| Field (on `FunctionType`) | Role |
| --- | --- |
| `abi` | e.g. `Source`, `BindC`, `Intrinsic` |
| `deftype` | `Implementation` or `Interface` |
| `bindc_name` | BindC export name from `bind(C, name=...)` only |
| `module` | true if this is a module procedure |
| `pure` / `elemental` / … | procedure attributes |

Naming for code generation is **not** solely `name`:

```text
if BindC and bindc_name set → use bindc_name
else if link_name set         → use link_name
else                          → use name (+ normal mangling rules)
```

Backends must not invent external names and must not recover them by
string-stripping conventions encoded in `name`.

## link_name

### When `name` and the link symbol diverge

A specific procedure that shares its generic interface’s name cannot occupy the
same symbol-table key as the `GenericProcedure`. LFortran stores that specific
under `"<name>~genericprocedure"`, so `Function.name` is a **disambiguated
key**, not necessarily the **external linkage name**.

- For **module procedures**, definition and call both go through LFortran’s
  mangling of the internal `name`; `link_name` stays null. This includes a
  `module function` / `module subroutine` interface body inside a self-named
  generic: it is an interface, but its implementation is a module procedure
  reached through the mangled key, not an external symbol.
- For **external interface bodies**, calls must link to the real external
  symbol (`"<name>"`), which may be defined in another translation unit or by
  another compiler. Semantics sets `link_name` to that external name when it
  creates the disambiguated interface Function.

`link_name` is independent of BindC: `FunctionType.bindc_name` remains the
BindC export name only.

### Invariants

ASR verification enforces, for any `Function`:

- `link_name` is either **null** or a **non-empty** string. An empty string is
  invalid; use null when the link name equals `name`.
- If `link_name` is non-null then `FunctionType.deftype == Interface` and
  `FunctionType.module == false`.

The second rule is what makes the field safe to trust: only a procedure defined
elsewhere has an external linkage name. A module procedure links through
LFortran's mangling of the (possibly disambiguated) symbol table key, and an
implementation defines its own symbol, so a `link_name` on either would make a
backend emit or call the wrong symbol. Given these rules a backend can use a
non-null `link_name` directly, as a non-empty C string, with no further checks.

- If `FunctionType.deftype == Interface`, `module == false`, and `name` ends
  with `~genericprocedure`, then `link_name` must be present and equal to
  `name` without that suffix.

The disambiguated key and `link_name` are kept consistent by
`self_named_generic_link_name()` in
`src/lfortran/semantics/ast_symboltable_visitor.cpp`, the single place that
decides whether a self-named generic specific gets a `link_name`.

### Who sets it

AST→ASR (semantics) sets `link_name` only when it differs from `name` (today:
self-named external interface bodies). Other construction paths leave it null.

## Examples

### Self-named external interface

```fortran
module m
  interface get_text
    integer function get_text()
    end function get_text
  end interface
end module
```

The specific is stored as `get_text~genericprocedure` with
`link_name = "get_text"`. Calls through the generic link as `get_text`, not as
the disambiguated symbol-table key.

### Ordinary procedure

```fortran
integer function f(x)
  integer, intent(in) :: x
  f = x + 1
end function
```

`name = "f"`, `link_name` is null; backends use `name` with normal mangling.

## See also

[GenericProcedure](symbol.md), [Variable](Variable.md),
[ExternalSymbol](ExternalSymbol.md), [FunctionCall](../expression_nodes/FunctionCall.md).
