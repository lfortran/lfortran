# SetContains

Whether a set contains a value.

## Declaration

### Syntax

```text
SetContains(expr left, expr right, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the set. |
| `right` | the value looked for. |
| `type` | the logical type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`x in s`. This is the operation a set exists for, and it does not depend on
the number of elements the way [ListContains](ListContains.md) does.

## Examples

```clojure
(SetContains
  :left (Var
    :v (SymbolRef 1 "s")
  )
  :right (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :type (Logical
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

[Set](../type_nodes/Set.md), [ListContains](ListContains.md), [DictContains](DictContains.md)
