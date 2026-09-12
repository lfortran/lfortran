# ImplicitDeallocate

Releases local allocatables when a scope is left.

## Declaration

### Syntax

```text
ImplicitDeallocate(expr* vars)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `vars` | the allocatables to release. |

### Return values

None.

## Description

A local allocatable is deallocated automatically when its procedure returns,
and the frontend makes that explicit by putting an **ImplicitDeallocate** at
the end of the scope. Unlike [ExplicitDeallocate](ExplicitDeallocate.md) it
does nothing for an object that is not allocated, since reaching the end of a
scope says nothing about which branches ran.

## Examples

```clojure
(ImplicitDeallocate
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

[ExplicitDeallocate](ExplicitDeallocate.md), [Allocate](Allocate.md)
