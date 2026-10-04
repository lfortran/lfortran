# GlobalInitDispatch

Enters the startup engine at the start of a main program.

## Declaration

### Syntax

```text
GlobalInitDispatch()
```

### Arguments

None.

### Return values

None.

## Description

**GlobalInitDispatch** is where generated code enters the one startup engine
that runs every startup initializer of the program before anything can
observe the state it initializes; see the *Startup initializers* section of
[Program](../symbol_nodes/Program.md). The `global_init_wire` pass makes it
the first statement of every program. A main program is the collective
boundary at which every image runs the same startup before its first
statement: the initializers that need no collective boundary, then the
collective bootstraps that start the coarray runtime, then the initializers
of saved coarrays. It appears only as a top-level statement of the body of a
[Program](../symbol_nodes/Program.md), at most once, and `asr_verify`
rejects it anywhere else.

No procedure dispatches, `bind(c)` procedures included, so a call costs
nothing for the startup. A program whose main function is not Fortran enters
the same collective boundary by calling `lfortran_initialize` before it calls
any Fortran procedure, as LFortran's `ISO_Fortran_binding.h` describes;
object files also enter the engine, for the initializers that need no
collective boundary, from their constructors.

A backend whose output is linked with other code lowers it to a call of the
runtime engine, `_lcompilers_init_dispatch` with the collective phase. A
backend whose output is the whole program lowers it through
`ASRUtils::expand_closed_world_dispatch` instead, into calls of every
initializer the program has.

## Examples

```clojure
(GlobalInitDispatch)
```

It comes from this complete ASR text document, the output of the
`global_init` and `global_init_wire` passes:

```{literalinclude} ../../examples/global_init.asr
:language: clojure
```

## See Also

[GlobalInitStorage](GlobalInitStorage.md), [Program](../symbol_nodes/Program.md), [Module](../symbol_nodes/Module.md)
