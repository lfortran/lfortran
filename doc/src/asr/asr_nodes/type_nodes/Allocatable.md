# Allocatable

An allocatable of another type.

## Declaration

### Syntax

```text
Allocatable(ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `type` | the type the object has once it is allocated. |

### Return values

None. A type is not evaluated.

## Description

An allocatable has no storage until
[Allocate](../statement_nodes/Allocate.md) gives it some, and it is
deallocated automatically when its scope ends. That is the difference from
[Pointer](Pointer.md), which may refer to storage it does not own.

The wrapped type carries the shape and the length: an allocatable array is an
**Allocatable** of an [Array](Array.md) whose extents are `nil`.

## Examples

```clojure
(Allocatable
  :type (Array
    :type (Integer
      :kind 4
    )
    :dims [
      (dimension
        :start nil
        :length nil
      )
    ]
    :physical_type :DescriptorArray
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/allocate_stmt.asr
:language: clojure
```

## See Also

[Pointer](Pointer.md), [Allocate](../statement_nodes/Allocate.md), [ExplicitDeallocate](../statement_nodes/ExplicitDeallocate.md), [Array](Array.md)
