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

Where an initializer lives is not a choice between two equally good forms. A
declaration initializer a target *can* lay out as static data — an integer or
character constant, `=> null()`, anything on a `parameter` — stays on the
declaration and is never moved. Only the ones it cannot are moved, and moving
one means taking it off the declaration: after the pass, a variable carries
its initializer in exactly one of the two places, never in both. Both places
being filled is the compiler contradicting itself about who initializes the
variable, and a backend that then leaves the setup to an initializer it does
not emit produces a program that reads uninitialized memory.

The two forms do both occur before the pass runs: what semantics produces has
every initializer on its declaration, which is the state `global_init`
consumes. The rule above is that pass's postcondition, not something
`asr_verify` can check on its own, since it cannot know
which passes have already run.

The body of an initializer is one guarded block, so calling it again does
nothing:

```text
if (.not. already_run) then
    already_run = .true.
    <the initialization statements>
end if
```

Nothing calls one of them twice — see the ordering below — so under `--fast`
the guard is dropped and the body is the initialization statements
themselves. The guard at the top of a procedure or block body is kept in
every mode: that one is the save attribute of an initialized local, so it
decides what the program does rather than repeating a call that cannot
happen.

Ordering is therefore expressed in ASR, not left to a target: a program's own
initializer calls every module initializer it can observe, once each and in
module dependency order, before the program's first statement — which is the
single call the program body gains. That list is complete rather than a list
of the program's direct dependencies: a module the program reaches only
through another module is in it too, so no module initializer needs to call
any other. An initializer defined in another object file is called through an
[ExternalSymbol](ExternalSymbol.md) like any other module procedure.

That chain is rooted at the program, so it reaches nothing at all when no
program is linked — a C driver calling a `bind(c)` module procedure,
LFortran's output used as a library — and it cannot reach a module the
program is unable to observe, as it cannot reach one that only a separately
compiled external procedure uses. A backend therefore also runs a
module's own initializer from the startup hook of the object file that
*defines* the module, which is the one object file linked wherever the
module's storage is. That is a second path to the same initializer, not a
second initialization: the run-once guard above is what makes it one, which
is why a module initializer keeps its guard even under `--fast`, where a
program's and the translation unit's are dropped. The LLVM backend leaves one
case out for now: a module that declares an array pointer or an allocatable
array is still left to the call chain alone, because the companion descriptor
of such a variable is a frame slot of `main` and the initializer has to run
after it is filled in.

The two paths cannot disagree about order. What a module initializer holds is
a pointer association, a broadcast of constants, or the allocation of a saved
coarray: an address and a constant in the first two, which no other
initializer can change, and in the third a collective that every image reaches
in the order of the one binary they all run. None of them can observe whether
another module's initializer has run, so running them in link order rather
than in dependency order is not something a program can tell apart. When a
main program is there the call chain still runs, before the program's first
statement and in dependency order, every initializer that has somehow not run
yet.

A backend therefore needs no support for *calling* module and program
initializers — the ASR calls those where Fortran says they run — only for
[TranslationUnit](../unit_nodes/TranslationUnit.md)`.global_init`, which no
ASR statement calls, and for registering a module's own with the target's
startup.

Putting those calls in is a second pass, `global_init_wire`, because a pass
that runs later than `global_init` can create an initializer too — `coarray`
does, for saved coarrays — and an initializer nothing calls would never run.

Which unit's initializer a saved coarray goes into is not decided by where it
is declared but by which program unit encloses that declaration, because
allocating a coarray is collective and so cannot wait until control first
reaches the procedure that declares it. The `coarray` pass walks outwards from
the declaration to the first enclosing **Program** or [Module](Module.md) and
uses that unit's initializer, so a saved coarray of a module procedure is
allocated by the module's and one of an internal procedure by the program's,
exactly as if it had been declared in that unit directly. Only a saved coarray
of an *external* procedure, which no program unit encloses, is left on the
[TranslationUnit](../unit_nodes/TranslationUnit.md) — which is why that one
case still needs the target's startup hook.

Under separate compilation a saved coarray follows the same rule as an
ordinary declaration initializer. A module read from a `.mod` file was
compiled into an object file of its own, and that object file allocates the
module's saved coarrays and binds each one to its companions. A translation
unit that only *uses* such a module therefore names that initializer without
defining a second one — defining one would clash at link time with the
definition already there — and leaves the coarrays themselves alone. Binding
them again here would point the module's coarrays at companions this unit
allocated rather than at the storage every image agreed on, which is why the
two go together: skipping the definition without skipping the binding would
replace a good association with a private one.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/program.asr
:language: clojure
```

## See Also

[TranslationUnit](../unit_nodes/TranslationUnit.md), [Module](Module.md), [Function](Function.md), [Variable](Variable.md)
