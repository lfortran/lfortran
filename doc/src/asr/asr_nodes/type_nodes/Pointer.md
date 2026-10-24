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

A pointer array is a **Pointer** of an [Array](Array.md) whose extents are
`nil`. Pointer association sets the bounds and the strides at run time, so
that array always has physical type `DescriptorArray` (`AssumedRankArray` when
its rank is assumed); the verifier reports any other as
`asr.verify.pointer.array_descriptor`.

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
