# CPtr

A C pointer: an address with no type and no shape.

## Declaration

### Syntax

```text
CPtr()
```

### Arguments

None.

### Return values

None. A type is not evaluated.

## Description

`type(c_ptr)` from `iso_c_binding`. It carries an address and nothing else, so
nothing can be read through it until
[CPtrToPointer](../statement_nodes/CPtrToPointer.md) supplies the type and the
shape.

## Examples

```clojure
(CPtr)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/cptr_expr.asr
:language: clojure
```

## See Also

[CLoc](../expression_nodes/CLoc.md), [CPtrToPointer](../statement_nodes/CPtrToPointer.md), [PointerToCPtr](../expression_nodes/PointerToCPtr.md), [Pointer](Pointer.md)
