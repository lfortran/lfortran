# CustomOperator

A user-defined or overloaded operator.

## Declaration

### Syntax

```text
CustomOperator(symbol_table parent_symtab, identifier name,
    symbol* procs, access access)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `parent_symtab` | the symbol table this symbol is stored in. |
| `name` | the operator name, such as `~add` for `operator(+)` or `~assign` for `assignment(=)`. |
| `procs` | the procedures that implement the operator. |
| `access` | `Public` or `Private`. |

### Return values

None.

## Description

**CustomOperator** holds the procedures declared in an
`interface operator(.op.)`, `interface operator(+)` or `interface
assignment(=)` block. Like [GenericProcedure](GenericProcedure.md) it is only
a symbol: the frontend resolves an operator use to one specific procedure and
emits an ordinary call, wrapped in an
[OverloadedBinOp](../expression_nodes/OverloadedBinOp.md),
[OverloadedCompare](../expression_nodes/OverloadedCompare.md) or the
`overloaded` member of
[Assignment](../statement_nodes/Assignment.md), so that the original spelling
is not lost.

## Examples

```clojure
(CustomOperator
  :parent_symtab 1
  :name "~add"
  :procs [
    (SymbolRef 1 "plus")
  ]
  :access :Public
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/customoperator.asr
:language: clojure
```

## See Also

[GenericProcedure](GenericProcedure.md), [OverloadedBinOp](../expression_nodes/OverloadedBinOp.md), [OverloadedCompare](../expression_nodes/OverloadedCompare.md), [Assignment](../statement_nodes/Assignment.md)
