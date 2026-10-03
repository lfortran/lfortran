# GlobalInitStorage

Creates the run-time storage of variables, storing no value.

## Declaration

### Syntax

```text
GlobalInitStorage(expr* targets)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `targets` | `Var`s of the variables whose storage a backend creates at run time, none of them `bind(c)`: variables of the owner of the enclosing startup initializer, and for the translation unit's initializer also variables of a module without an initializer of its own, reached through an `ExternalSymbol` in the initializer's symbol table. |

### Return values

None.

## Description

**GlobalInitStorage** is the *physical setup* of the storage of a module's or
the translation unit's variables: what a backend's representation of the
storage needs before a value can be stored in it, such as a descriptor for an
array component, a buffer for a character component or a type pointer. It
stores no value; the statements after it do.

The `global_init_wire` pass puts exactly one into every startup initializer
defined in the translation unit, inside its guard, right after the calls of
the initializers it depends on and before its own statements, so that the
initializer establishes storage and values together, once. Its targets are the
variables of the owner for which `ASRUtils::needs_runtime_storage_setup`
holds: variables without an initializer that are neither allocatable nor
pointers, and are a derived-type scalar, or a fixed-size array of one, whose
type needs its components set up. The translation unit's initializer also
lists such variables of every module that has no initializer of its own. A
program's lists none, since its frame is set up where the program runs. The
list can be empty, but the statement is always there.

A backend lowers it to nothing only where the declaration it emits for a
target already is all of that target's storage, and otherwise creates the
storage there. A backend that cannot create a target's storage reports that
as an error instead of leaving the storage uncreated.

## Examples

```clojure
(GlobalInitStorage
  :targets [
    (Var
      :v (SymbolRef 3 "x")
    )
  ]
)
```

It comes from this complete ASR text document, where `x` is a module variable
of a derived type with a character component:

```{literalinclude} ../../examples/global_init.asr
:language: clojure
```

## See Also

[GlobalInitDispatch](GlobalInitDispatch.md), [Module](../symbol_nodes/Module.md), [Program](../symbol_nodes/Program.md)
