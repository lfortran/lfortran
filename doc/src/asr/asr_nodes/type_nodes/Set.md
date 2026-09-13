# Set

An unordered collection of distinct values of one type.

## Declaration

### Syntax

```text
Set(ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `type` | the element type. |

### Return values

None. A type is not evaluated.

## Description

Membership, not order, is what a set is for:
[SetContains](../expression_nodes/SetContains.md) does not depend on the
number of elements the way a search through a list does. Iterating a set
visits its elements in an unspecified order.

## Examples

```clojure
(Set
  :type (Integer
    :kind 4
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/set_expr.asr
:language: clojure
```

## See Also

[List](List.md), [Dict](Dict.md), [SetContains](../expression_nodes/SetContains.md), [SetConstant](../expression_nodes/SetConstant.md)
