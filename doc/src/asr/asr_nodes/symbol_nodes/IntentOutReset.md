# IntentOutReset

How an object of one derived type is reset for an `intent(out)` dummy
argument.

## Declaration

### Syntax

```text
IntentOutReset(symbol_table parent_symtab, identifier name,
    symbol type_declaration, symbol? procedure)
```

`type_declaration` is a `Struct` of the same scope. An object of exactly that
type that becomes a nonpointer, nonallocatable `intent(out)` dummy argument is
finalized, has its allocatable components deallocated and is default
initialized on entry to the procedure (F2018 7.5.6.3, 8.5.10). `procedure`
does that in place: a private subroutine of the same scope whose only dummy is
such a `type(T), intent(out)` object, so that it resets the object exactly as
the start of any procedure with that dummy does. It is absent when the reset
has no effect, because neither the type nor a parent type has a final
subroutine and no component (also of a component of derived type) is
allocatable or has a default initialization, and when no object has the type
as its own, because it is abstract.

The `intent_out_deallocate` pass records every derived type of a translation
unit when `PassOptions::dynamic_intent_out_reset` is set: those of the unit,
of the modules it uses, of its programs, procedures with a body, `BLOCK`
constructs and generic instances, whether or not the unit has a polymorphic
dummy of them, because another unit can pass an object of the type to one.
Generic definitions, traits and interface bodies have no objects of their own
types and are not recorded. A type that already has a record keeps it, and
the pass knows a reset procedure by its record. The records are made before
the procedures are visited, so the procedures take part in dependencies,
verification and the passes after it like any other procedure.

The LLVM code generator sets that option and resets a scalar polymorphic
`intent(out)` dummy (other than `class(*)`) from its dynamic type: on entry it
calls the function in a fixed slot of the dynamic type's vtable with the
object's address. The slot holds the record's procedure, which every unit
that builds the vtable defines for itself with internal linkage, or null when
the record has none. A derived type that the pass records but that has no
record when its vtable is built is an internal error.

Verification checks that the record names a derived type of its own scope,
that each type has one record, that the procedure is present exactly when the
reset of a type that is not abstract has an effect, and that the procedure is
a private subroutine of the same scope with the one `intent(out)` dummy of
exactly that type.
