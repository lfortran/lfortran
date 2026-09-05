# SymbolicExpression

A symbolic algebra expression.

## Declaration

### Syntax

```text
SymbolicExpression()
```

### Arguments

None.

### Return values

None. A type is not evaluated.

## Description

LPython's `S` type: a value that is an unevaluated expression rather than a
number, handled by a computer algebra library at run time. It has no
parameters, since every symbolic expression has the same type.

## Examples

```clojure
(SymbolicExpression)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/symbolic_expr.asr
:language: clojure
```

## See Also

[SymbolicCompare](../expression_nodes/SymbolicCompare.md)
