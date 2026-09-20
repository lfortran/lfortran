# Program

The main program: the entry point of an executable.

## Declaration

### Syntax

```text
Program(symbol_table symtab, identifier name, identifier* dependencies,
    stmt* body, identifier? global_init, location start_name,
    location end_name)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the symbol table of the program. It owns the local variables of the program and the `ExternalSymbol` entries for the module symbols the program uses. |
| `name` | the name of the program. |
| `dependencies` | the names of the modules and procedures the body of the program refers to. The backends use it to order code generation. |
| `body` | the statements of the program, in order. |
| `global_init` | the name of this program's startup initializer in `symtab`, or `nil`. See the description below. |
| `start_name` | the source span of the name in `program name`. |
| `end_name` | the source span of the name in `end program name`, or an empty span when the end statement does not repeat it. |

### Return values

None.

## Description

A translation unit that is linked into an executable must contain exactly one
**Program**. It is always owned by the global symbol table.

A program has no arguments and no return value, so it needs no
`function_signature`: unlike [Function](Function.md) it can never be called
from ASR.

Statements typed directly into the REPL, or written outside of any program
unit, first appear in `TranslationUnit.items`. The `global_stmts` ASR pass
wraps them in a **Program** so that the rest of the compiler only has to deal
with program units.

### Startup initializers

A declaration initializer that no target can lay out as static data — a
derived-type array broadcast such as `type(t) :: a(3) = t(7)`, a pointer
association such as `integer, pointer :: p => tgt`, the allocation of a saved
coarray — has to run as executable statements before any code can observe the
variable. The `global_init` ASR pass moves such an initializer out of the
declaration and into a **startup initializer**: an argument-less procedure in
the owner's own symbol table, named by the owner's `global_init`. A
[Module](Module.md) and a **Program** each name at most one, which is why the
link lives on the owner rather than on the procedure.

A procedure or a block needs no initializer procedure of its own: nothing
outside it can observe its variables, so there is nobody to call one. The same
pass puts their initialization straight into a guarded block at the top of
their own body, which is what the save attribute Fortran gives every
initialized local means.

The body of an initializer is one guarded block, so calling it again does
nothing:

```text
if (.not. already_run) then
    already_run = .true.
    <calls to the initializers of the modules this one uses>
    <the initialization statements>
end if
```

Ordering is therefore expressed in ASR, not left to a target: a module
initializer calls the initializers of the modules it uses, and a program's own
initializer calls every module initializer it can observe, in module
dependency order, before the program's first statement — which is the single
call the program body gains. Neither link order nor a constructor priority can
change the order Fortran requires, and an initializer defined in another
object file is called through an [ExternalSymbol](ExternalSymbol.md) like any
other module procedure. A backend therefore needs no support at all for module
and program initializers — only for
[TranslationUnit](../unit_nodes/TranslationUnit.md)`.global_init`, which no
ASR statement calls.

Putting those calls in is a second pass, `global_init_wire`, because a pass
that runs later than `global_init` can create an initializer too — `coarray`
does, for the saved coarrays of a module or a program — and an initializer
nothing calls would never run.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/program.asr
:language: clojure
```

## See Also

[TranslationUnit](../unit_nodes/TranslationUnit.md), [Module](Module.md), [Function](Function.md), [Variable](Variable.md)
