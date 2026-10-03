# cptr_kind

The ISO C binding pointer identity carried by [CPtr](../type_nodes/CPtr.md).

## Declaration

### Syntax

```text
cptr_kind = CPointer | CFunPointer | CPtrUnspecified
```

### Values

| Value | Description |
|-------|-------------|
| `CPointer` | A `type(c_ptr)` value. |
| `CFunPointer` | A `type(c_funptr)` value. |
| `CPtrUnspecified` | A C pointer value whose ISO C binding identity is not relevant or not known. |

## Description

The frontend sets this after resolving the real intrinsic `iso_c_binding`
symbols, so user-defined derived types named `c_ptr` or `c_funptr` are not
mistaken for C pointer types.

## See Also

[CPtr](../type_nodes/CPtr.md), [PointerToCPtr](../expression_nodes/PointerToCPtr.md)
