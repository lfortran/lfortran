# ListAppend

Appends an element to a list.

## Declaration

### Syntax

```text
ListAppend(expr a, expr ele)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the list. |
| `ele` | the element to append. |

### Return values

None.

## Description

**ListAppend** is LPython's `a.append(x)`. It grows the list by one element,
reallocating it when necessary; Fortran has no equivalent, since a Fortran
array has a fixed shape until it is reallocated explicitly.

## Examples

```clojure
(ListAppend
  :a (Var
    :v (SymbolRef 1 "a")
  )
  :ele (IntegerConstant
    :n 4
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

[ListInsert](ListInsert.md), [ListRemove](ListRemove.md), [ListClear](ListClear.md), [List](../type_nodes/List.md)
