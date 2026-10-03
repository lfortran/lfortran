# SetRemove

Removes an element from a set.

## Declaration

### Syntax

```text
SetRemove(expr a, expr ele)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the set. |
| `ele` | the element to remove. |

### Return values

None.

## Description

Removing an element the set does not contain is an error at run time.

## Examples

```clojure
(SetRemove
  :a (Var
    :v (SymbolRef 1 "s")
  )
  :ele (IntegerConstant
    :n 2
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

[SetInsert](SetInsert.md), [SetClear](SetClear.md), [SetContains](../expression_nodes/SetContains.md)
