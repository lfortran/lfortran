# PointerAssociated

Whether a pointer is associated, or associated with a particular target.

## Declaration

### Syntax

```text
PointerAssociated(expr ptr, expr? tgt, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `ptr` | the pointer to test. |
| `tgt` | the target to compare against, or `nil` to test only whether the pointer is associated. |
| `type` | the logical type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`associated(p)` and `associated(p, t)`. With a target the test is whether the
pointer refers to that particular object, which is not the same question as
whether it refers to anything.

## Examples

```clojure
(PointerAssociated
  :ptr (Var
    :v (SymbolRef 1 "p")
  )
  :tgt (Var
    :v (SymbolRef 1 "target")
  )
  :type (Logical
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/cptr_expr.asr
:language: clojure
```

## See Also

[Nullify](../statement_nodes/Nullify.md), [PointerNullConstant](PointerNullConstant.md), [GetPointer](GetPointer.md)
