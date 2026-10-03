# SetPop

Removes and returns an element of a set.

## Declaration

### Syntax

```text
SetPop(expr a, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the set. |
| `type` | the element type. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`s.pop()`. A set has no order, so which element is removed is unspecified.
It is an expression rather than a statement because the removed element is
its value; [SetRemove](../statement_nodes/SetRemove.md) is the statement that
removes a particular one.

## Examples

```clojure
(SetPop
  :a (Var
    :v (SymbolRef 1 "s")
  )
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/set_expr.asr
:language: clojure
```

## See Also

[SetRemove](../statement_nodes/SetRemove.md), [DictPop](DictPop.md), [Set](../type_nodes/Set.md)
