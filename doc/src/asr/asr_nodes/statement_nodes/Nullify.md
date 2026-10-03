# Nullify

Disassociates pointers.

## Declaration

### Syntax

```text
Nullify(expr* vars)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `vars` | the pointers to disassociate. |

### Return values

None.

## Description

`nullify(p)` makes `p` point at nothing, which is not the same as deallocating
what it pointed at. Afterwards
[PointerAssociated](../expression_nodes/PointerAssociated.md) is false for it.

## Examples

```clojure
(Nullify
  :vars [
    (Var
      :v (SymbolRef 1 "p")
    )
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/allocate_stmt.asr
:language: clojure
```

## See Also

[Associate](Associate.md), [PointerNullConstant](../expression_nodes/PointerNullConstant.md), [PointerAssociated](../expression_nodes/PointerAssociated.md), [ExplicitDeallocate](ExplicitDeallocate.md)
