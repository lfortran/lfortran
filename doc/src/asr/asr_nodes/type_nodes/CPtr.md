# CPtr

A C pointer: an address with no type and no shape.

## Declaration

### Syntax

```text
CPtr(cptr_kind kind)
```

### Arguments

`kind` is `CPointer`, `CFunPointer`, or `CPtrUnspecified`, distinguishing `c_ptr` from `c_funptr` once semantics has resolved the intrinsic symbols.

### Return values

None. A type is not evaluated.

## Description

`type(c_ptr)` or `type(c_funptr)` from `iso_c_binding`. It carries an address and optional identity for which C pointer type it is, but no pointee type or shape, so
nothing can be read through it until
[CPtrToPointer](../statement_nodes/CPtrToPointer.md) supplies the type and the
shape.

## Examples

```clojure
(CPtr
  :kind :CPtrUnspecified
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/cptr_expr.asr
:language: clojure
```

## See Also

[CLoc](../expression_nodes/CLoc.md), [CPtrToPointer](../statement_nodes/CPtrToPointer.md), [PointerToCPtr](../expression_nodes/PointerToCPtr.md), [Pointer](Pointer.md)
