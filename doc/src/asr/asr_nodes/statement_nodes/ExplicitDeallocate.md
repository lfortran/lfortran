# ExplicitDeallocate

The `deallocate` statement written by the user.

## Declaration

### Syntax

```text
ExplicitDeallocate(expr* vars)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `vars` | the allocatables and pointers to deallocate. |

### Return values

None.

## Description

Deallocating something that is not allocated is an error at run time. That is
the difference from [ImplicitDeallocate](ImplicitDeallocate.md), which the
compiler inserts and which does nothing in that case.

## Examples

```clojure
(ExplicitDeallocate
  :vars [
    (Var
      :v (SymbolRef 1 "a")
    )
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/allocate_stmt.asr
:language: clojure
```

## See Also

[ImplicitDeallocate](ImplicitDeallocate.md), [Allocate](Allocate.md), [Nullify](Nullify.md)
