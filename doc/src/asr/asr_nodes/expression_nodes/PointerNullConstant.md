# PointerNullConstant

A pointer that refers to nothing.

## Declaration

### Syntax

```text
PointerNullConstant(ttype type, expr? var_expr)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `type` | the pointer type of the result. |
| `var_expr` | the pointer whose type the result takes, when `null()` was written with an argument; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`null()`. It is used as the right hand side of a pointer assignment and as the
initialiser of a pointer component, so that a pointer starts out
disassociated rather than undefined.

## Examples

```clojure
(PointerNullConstant
  :type (Pointer
    :type (Integer
      :kind 4
    )
  )
  :var_expr nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/pointernullconstant.asr
:language: clojure
```

## See Also

[Nullify](../statement_nodes/Nullify.md), [PointerAssociated](PointerAssociated.md), [Associate](../statement_nodes/Associate.md), [Pointer](../type_nodes/Pointer.md)
