# IntentOutReset

Gives a polymorphic INTENT(OUT) dummy argument what its dynamic type gets on
entry.

## Declaration

### Syntax

```text
IntentOutReset(expr var)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `var` | a scalar polymorphic variable, or an element of a polymorphic array, that is a nonpointer, nonallocatable INTENT(OUT) dummy argument. |

### Return values

None.

## Description

On entry to a procedure, an INTENT(OUT) dummy argument becomes undefined,
except that its allocatable components are deallocated and its components
with default initialization get their default values (F2018 8.5.10). The
dynamic type of a polymorphic dummy decides which components those are, and
it may be an extension of the declared type defined in another compilation
unit. **IntentOutReset** applies this according to the dynamic type of
`var`: the allocatable components of that type, those it inherits included,
are deallocated and left unallocated, and then the components that have
default initialization get their default values. It calls no final
subroutine.

The `intent_out_deallocate` pass puts an **IntentOutReset** at the start of
a procedure for every such dummy argument, for every element of an array
one, after the statements it emits for the declared type, and guards it with
`present()` for an optional dummy argument. A backend with virtual tables
lowers it to a call of the function the virtual table of the dynamic type
has for it. A backend without them does nothing for it, leaving only what
the pass emits for the declared type.

`var` must have a polymorphic (`class`) type and be a scalar `Var` or
`ArrayItem`.

## Examples

```clojure
(IntentOutReset
  :var (Var
    :v (SymbolRef 5 "v")
  )
)
```

## See Also

[ExplicitDeallocate](ExplicitDeallocate.md), [ImplicitDeallocate](ImplicitDeallocate.md)
