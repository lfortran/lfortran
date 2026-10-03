# ListInsert

Inserts an element at a position in a list.

## Declaration

### Syntax

```text
ListInsert(expr a, expr pos, expr ele)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the list. |
| `pos` | the index to insert at. |
| `ele` | the element to insert. |

### Return values

None.

## Description

Everything from `pos` onwards moves up by one. Inserting at the length of the
list appends to it.

## Examples

```clojure
(ListInsert
  :a (Var
    :v (SymbolRef 1 "a")
  )
  :pos (IntegerConstant
    :n 0
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :ele (IntegerConstant
    :n 0
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

[ListAppend](ListAppend.md), [ListRemove](ListRemove.md), [List](../type_nodes/List.md)
