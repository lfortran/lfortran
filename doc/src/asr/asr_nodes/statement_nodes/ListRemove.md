# ListRemove

Removes the first occurrence of a value from a list.

## Declaration

### Syntax

```text
ListRemove(expr a, expr ele)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the list. |
| `ele` | the value to remove. |

### Return values

None.

## Description

The element is found by value, not by index, and only the first match is
removed. Removing a value that is not in the list is an error at run time.

## Examples

```clojure
(ListRemove
  :a (Var
    :v (SymbolRef 1 "a")
  )
  :ele (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/list_expr.asr
:language: clojure
```

## See Also

[ListInsert](ListInsert.md), [ListAppend](ListAppend.md), [ListClear](ListClear.md)
