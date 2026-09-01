# Function

A function or a subroutine, with or without a body.

## Declaration

### Syntax

```text
Function(symbol_table symtab, identifier name,
    ttype function_signature, identifier* dependencies, expr* args,
    stmt* body, expr? return_var, access access, bool deterministic,
    bool side_effect_free, string? module_file, location start_name,
    location end_name)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the symbol table of the procedure: its dummy arguments, its result variable and its local variables. |
| `name` | the name of the procedure. |
| `function_signature` | a [FunctionType](../type_nodes/FunctionType.md) giving the argument types, the result type (`nil` for a subroutine), the ABI and the attributes (`elemental`, `pure`, `module`, ...). |
| `dependencies` | the names of the symbols the body refers to. |
| `args` | the dummy arguments, as [Var](../expression_nodes/Var.md) expressions pointing into `symtab`. The order is the calling order. |
| `body` | the statements of the procedure. Empty for an interface (`deftype=Interface`) and for an external procedure. |
| `return_var` | the result variable of a function, as a `Var`; `nil` for a subroutine. |
| `access` | `Public` or `Private`, from the module's access specification. |
| `deterministic` | `true` when the procedure returns the same result for the same arguments. Reserved for optimizations that need to duplicate or eliminate calls. |
| `side_effect_free` | `true` when a call has no effect other than its result. Reserved for the same optimizations. |
| `module_file` | the module file the procedure was loaded from, when it was; `nil` otherwise. |
| `start_name` | the source span of the name in the `function` or `subroutine` statement. |
| `end_name` | the source span of the name in the matching `end` statement. |

### Return values

None.

## Description

One constructor represents both Fortran functions and Fortran subroutines. A
subroutine is a **Function** whose `return_var` is `nil` and whose
`function_signature` has no `return_var_type`; it is called by
[SubroutineCall](../statement_nodes/SubroutineCall.md) rather than by
[FunctionCall](../expression_nodes/FunctionCall.md).

The dummy arguments appear twice: in `symtab`, which owns the
[Variable](Variable.md) symbols, and in `args`, which fixes their order. The
result variable of a function is likewise owned by `symtab`, with
`intent=ReturnVar`, and referenced by `return_var`.

`deterministic` and `side_effect_free` are declarations about the procedure,
not consequences of its body. A frontend that cannot prove them must leave
them `false`.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/function.asr
:language: clojure
```

## See Also

[Module](Module.md), [Variable](Variable.md), [FunctionCall](../expression_nodes/FunctionCall.md), [SubroutineCall](../statement_nodes/SubroutineCall.md), [FunctionType](../type_nodes/FunctionType.md)
