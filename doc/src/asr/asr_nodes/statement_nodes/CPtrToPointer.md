# CPtrToPointer

Converts a C pointer into a Fortran pointer.

## Declaration

### Syntax

```text
CPtrToPointer(expr cptr, expr ptr, expr? shape, expr? lower_bounds)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `cptr` | the `c_ptr` value to convert. |
| `ptr` | the Fortran pointer that is made to point at it. |
| `shape` | the shape the pointer takes when it is an array pointer. |
| `lower_bounds` | the lower bounds of that shape; `nil` means all ones. |

### Return values

None.

## Description

This is `c_f_pointer`. It is a statement rather than an expression because it
associates a pointer, and because the shape of the target is supplied here
rather than being known from the C pointer, which carries no shape at all.

[PointerToCPtr](../expression_nodes/PointerToCPtr.md) is the other direction,
and [CLoc](../expression_nodes/CLoc.md) takes the address of a target.

## Examples

```clojure
(CPtrToPointer
  :cptr (Var
    :v (SymbolRef 1 "c")
  )
  :ptr (Var
    :v (SymbolRef 1 "p")
  )
  :shape (Var
    :v (SymbolRef 1 "shape")
  )
  :lower_bounds nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/cptrtopointer_stmt.asr
:language: clojure
```

## See Also

[PointerToCPtr](../expression_nodes/PointerToCPtr.md), [CLoc](../expression_nodes/CLoc.md), [CPtr](../type_nodes/CPtr.md), [Associate](Associate.md)
