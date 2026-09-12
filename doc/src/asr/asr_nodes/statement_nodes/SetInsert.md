# SetInsert

Adds an element to a set.

## Declaration

### Syntax

```text
SetInsert(expr a, expr ele)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the set. |
| `ele` | the element to add. |

### Return values

None.

## Description

Adding an element the set already contains does nothing, which is what makes
the operation different from [ListAppend](ListAppend.md).

## Examples

```clojure
(SetInsert
  :a (Var
    :v (SymbolRef 1 "s")
  )
  :ele (IntegerConstant
    :n 3
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/set_expr.asr
:language: clojure
```

## See Also

[SetRemove](SetRemove.md), [SetClear](SetClear.md), [Set](../type_nodes/Set.md), [SetContains](../expression_nodes/SetContains.md)
