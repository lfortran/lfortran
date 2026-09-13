# GenericProcedure

A name that resolves to one of several procedures.

## Declaration

### Syntax

```text
GenericProcedure(symbol_table parent_symtab, identifier name,
    symbol* procs, access access)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `parent_symtab` | the symbol table this symbol is stored in. |
| `name` | the generic name. |
| `procs` | the specific procedures the name can resolve to. |
| `access` | `Public` or `Private`. |

### Return values

None.

## Description

A **GenericProcedure** is the ASR form of a generic interface block and of a
generic type-bound procedure. It is a symbol, not a call: resolution happens
in the frontend, which picks the specific procedure from the actual arguments
and stores it in the `name` member of the call node. The generic symbol is
kept in `original_name` there, so a later pass can still tell that the call was
written generically.

A **GenericProcedure** is never called directly and never reaches a backend as
a call target.

## Examples

```clojure
(GenericProcedure
  :parent_symtab 1
  :name "show"
  :procs [
    (SymbolRef 1 "show_int")
    (SymbolRef 1 "show_real")
  ]
  :access :Public
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/genericprocedure.asr
:language: clojure
```

## See Also

[CustomOperator](CustomOperator.md), [Function](Function.md), [FunctionCall](../expression_nodes/FunctionCall.md), [SubroutineCall](../statement_nodes/SubroutineCall.md)
