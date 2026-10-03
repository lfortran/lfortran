# List

A list of values of one type, whose length varies at run time.

## Declaration

### Syntax

```text
List(ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `type` | the element type. |

### Return values

None. A type is not evaluated.

## Description

Unlike an [Array](Array.md) a list has no shape and no bounds: it grows and
shrinks with [ListAppend](../statement_nodes/ListAppend.md) and the other list
statements. Fortran has no such type; it is LPython's `list`.

## Examples

```clojure
(List
  :type (Integer
    :kind 4
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/list_expr.asr
:language: clojure
```

## See Also

[Array](Array.md), [Set](Set.md), [Tuple](Tuple.md), [ListConstant](../expression_nodes/ListConstant.md)
