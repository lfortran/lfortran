# Pointer

A pointer to another type.

## Declaration

### Syntax

```text
Pointer(ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `type` | the type of what the pointer refers to. |

### Return values

None. A type is not evaluated.

## Description

A pointer refers to storage owned by something else, or by nothing at all when
it is disassociated. It is associated by
[Associate](../statement_nodes/Associate.md), tested with
[PointerAssociated](../expression_nodes/PointerAssociated.md) and cleared by
[Nullify](../statement_nodes/Nullify.md).

## Examples

```clojure
(Pointer
  :type (Integer
    :kind 4
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/cptr_expr.asr
:language: clojure
```

## See Also

[Allocatable](Allocatable.md), [Associate](../statement_nodes/Associate.md), [Nullify](../statement_nodes/Nullify.md), [GetPointer](../expression_nodes/GetPointer.md), [CPtr](CPtr.md)
