# ListConcat

Two lists joined into a new one.

## Declaration

### Syntax

```text
ListConcat(expr left, expr right, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the first list. |
| `right` | the second list. |
| `type` | the list type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`a + b` in LPython. Neither operand is modified; the result is a new list.

## Examples

```clojure
(ListConcat
  :left (Var
    :v (SymbolRef 1 "a")
  )
  :right (Var
    :v (SymbolRef 1 "b")
  )
  :type (List
    :type (Integer
      :kind 4
    )
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/list_expr.asr
:language: clojure
```

## See Also

[ListRepeat](ListRepeat.md), [ListAppend](../statement_nodes/ListAppend.md), [List](../type_nodes/List.md)
