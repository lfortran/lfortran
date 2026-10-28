# ExplicitDeallocate

The `deallocate` statement written by the user.

## Declaration

### Syntax

```text
ExplicitDeallocate(expr* vars, expr? stat, expr? errmsg)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `vars` | the allocatables and pointers to deallocate. |
| `stat` | a variable receiving the status: zero on success, non-zero if an object is not allocated. With `stat` the program continues after a failed deallocation. |
| `errmsg` | a character variable receiving the error message. |

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
  :stat (Var
    :v (SymbolRef 1 "stat")
  )
  :errmsg nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/allocate_stmt.asr
:language: clojure
```

## See Also

[ImplicitDeallocate](ImplicitDeallocate.md), [Allocate](Allocate.md), [Nullify](Nullify.md)
