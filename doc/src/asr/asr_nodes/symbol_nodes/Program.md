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

Initializing a variable involves three separate questions, and keeping them
apart is what decides where each piece of the work belongs:

* **Semantic initialization** is the initial state the language defines: a
  declaration initializer, the default initialization of a derived type's
  components, a pointer association such as `p => tgt`, or the allocation
  Fortran requires for a saved coarray. ASR states it — on the declaration,
  in `Variable.symbolic_value` and the component defaults, or as the
  statements of a startup initializer, below.
* **Physical setup** is what a backend's representation of the storage needs
  before a value can be stored in it: a descriptor for an array component, a
  buffer for a character component, a type pointer. It follows from the ASR
  type and the backend's layout and adds nothing to the semantics.
* **Materialization** is whether an initial value is laid out as static data
  or produced by code that runs once at startup. It is a choice rather than
  part of the semantics: most initial values could be materialized either
  way, and the better choice can depend on size and target (see
  *Materialization policy* below).

Whichever way a program is put together — a Fortran main program, a C driver
calling `bind(c)` procedures, LFortran's output used as a library, separately
compiled object files — every variable has to be initialized exactly once,
before anything can observe it. Three rules follow from that:

* Code that runs at startup does only what the static data does not already
  hold. It never stores a static value back: static data is in place before
  any code runs, while startup code can run after user or library code has
  already changed the storage, and storing the initial value again would undo
  that change.
* ASR states the initial state; a target only carries it out. A target's
  startup hook (`@llvm.global_ctors` for LLVM) creates the storage the layout
  of a variable does not hold in place — the physical setup, which follows
  from the ASR types — and calls or registers the initializers ASR names. It
  never stores a value, and it does not work out what to initialize, or what
  to leave alone, by inspecting the instructions it generated.
* Initialization that depends on other code having run first — a call into a
  library, the collective allocation of a coarray — is ordered explicitly, by
  the call chain described below. The order in which a linker runs the
  constructors of different object files cannot be predicted from the source,
  and constructor priorities are not honored on every target (Mach-O runs the
  constructors of different object files in link order), so neither is
  something to rely on.

#### What is executable today

The `global_init` ASR pass moves a declaration initializer that the current
lowering does not materialize as static data out of the declaration and into
a **startup initializer**: an argument-less procedure in the owner's own
symbol table, named by the owner's `global_init`. A [Module](Module.md) and a
**Program** each name at most one, which is why the link lives on the owner
rather than on the procedure. After the pass a variable carries its
initializer in exactly one of the two places, never in both: both being
filled is the compiler contradicting itself about who initializes the
variable, and a backend that then leaves the setup to an initializer it does
not emit produces a program that reads uninitialized memory. What semantics
produces has every initializer on its declaration, which is the state
`global_init` consumes, so this is that pass's postcondition, not something
`asr_verify` can check on its own, since it cannot know which passes have
already run.

Today the pass lowers three kinds of initialization to statements, and never
one of a `parameter`:

* a pointer association, unless it is one of a module's scalar pointers and
  its target has a link-time address. `integer, pointer :: p => tgt` in a
  module is static data, the address of `tgt`; the descriptor of an array
  pointer, that of a character pointer such as `character(:), pointer :: q =>
  str` (which carries the length too), a polymorphic pointer, and a pointer
  declared in a program or a procedure are still associated by a statement;
* a derived-type array broadcast such as `type(t) :: a(3) = t(7)` whose
  element type has a component whose storage is created at run time — a
  character component, an allocatable, pointer or non-constant-size array, or
  a polymorphic component — because the current layout gives each element a
  buffer or descriptor of its own. When every component of the element is
  described by a constant, the broadcast stays on the declaration and is laid
  out as static data;
* the default initialization of a module variable of a derived type that has
  no declaration initializer, as far as static data does not hold it. A
  scalar one is laid out as static data holding every default
  `ASRUtils::struct_member_default_is_static` accepts, however deeply the
  derived types it holds by value nest. The pass adds a statement for every
  other default: one in storage created at run time, such as a character
  component's buffer, and a pointer component's initial procedure or target.
  An array of a derived type is laid out as zeros, whatever its size, and the
  pass gives its elements all their defaults in one loop over the array. The
  static layout and the pass walk the same defaults with the same predicate,
  so every component is initialized by exactly one of the two.

Everything else — including `=> null()` and every other constant — stays on
the declaration and is laid out as static data.

The `coarray` pass adds the one kind that is runtime-dependent by nature
under the current PRIF representation: a saved coarray. Its storage is
allocated and registered by a collective call into the PRIF implementation,
which provides the address the Fortran pointer is then bound to, and only
after that can the initial value be assigned. An initialization that uses the
coarray, such as `integer, pointer :: ptr => co_var`, has to follow the
allocation, which is why the pass puts the allocation ahead of everything else
in the initializer (`tests/coarray_initialization_01.f90` and
`integration_tests/coarrays_49.f90`). Because the allocation is collective, it
also cannot wait until control first reaches a procedure that declares a
saved coarray.

Most other cases are executable because of how they are lowered today, not
because static data could not describe them. A character pointer to a fixed
target has a link-time address and a constant length, and so does a pointer
component's initial target. A fixed-size array of a derived type, or a
character component, could be given distinct mutable static storage per
element or per object instead of a buffer set up at startup, and a null
pointer component a static null descriptor of its own, as `type(t) :: x =
t(null())` already gets. A test that exercises one of these through a startup
hook tests the hook, not the impossibility of static data.

A procedure or a block needs no initializer procedure of its own: nothing
outside it can observe its variables, so there is nobody to call one. The same
pass puts their initialization straight into a guarded block at the top of
their own body, which is what the save attribute Fortran gives every
initialized local means.

#### The call chain

The body of an initializer is one guarded block, so calling it again does
nothing:

```text
if (.not. already_run) then
    already_run = .true.
    <the initialization statements>
end if
```

Ordering is expressed in ASR, not left to a target: a program's own
initializer calls every module initializer it can observe, once each and in
module dependency order, before the program's first statement — which is the
single call the program body gains. That list is complete rather than a list
of the program's direct dependencies: a module the program reaches only
through another module is in it too, so no module initializer needs to call
any other. An initializer defined in another object file is called through an
[ExternalSymbol](ExternalSymbol.md) like any other module procedure.

Putting those calls in is a second pass, `global_init_wire`, because a pass
that runs later than `global_init` can create an initializer too — `coarray`
does, for saved coarrays — and an initializer nothing calls would never run.

A backend therefore needs no support for *calling* module and program
initializers — the ASR calls those where Fortran says they run — only for
[TranslationUnit](../unit_nodes/TranslationUnit.md)`.global_init`, which no
ASR statement calls, and for registering a module's own with the target's
startup, below.

#### The startup hook of the defining object file

The call chain is rooted at the program, so it reaches nothing at all when no
program is linked — a C driver calling a `bind(c)` module procedure,
LFortran's output used as a library — and it cannot reach a module the
program is unable to observe, as it cannot reach one that only a separately
compiled external procedure uses. A backend therefore also runs a module's
own initializer from the startup hook of the object file that *defines* the
module, which is the one object file linked wherever the module's storage is.
That is a second path to the same initializer, not a second initialization:
the run-once guard above is what makes it one.

Which initializers take that second path is stated in ASR, by
[Module](Module.md)`.global_init_at_startup`, and not worked out by a backend
from the shape of the body. The pass that puts a statement into an initializer
is the one that knows what the statement is, so it is the one that says so:
every entry point that adds to an initializer takes an `InitOrdering` beside
the statements.

`OrderInsensitive` is an assignment or an association that reads a constant or
the address of a variable with `save` — a declaration initializer, in other
words. Neither of those is something another initializer can change, so
running such an initializer in link order rather than in dependency order is
not something a program can tell apart. A module starts out with
`global_init_at_startup` set, because an empty body is order-insensitive, and
keeps it while only such statements go in.

`Ordered` is everything else, and a call above all, since a call from a
startup hook can reach a library whose own startup has not run yet. The
allocation of a saved coarray is exactly that: it is a call into the PRIF
implementation, which is itself built from Fortran modules with saved state of
its own. The `coarray` pass therefore adds it as `Ordered`, which takes
`global_init_at_startup` away for good, and the module keeps to the call
chain, which runs after everything the target starts up.

`global_init_at_startup` is also what says whether an initializer is reached
twice, so it decides whether the run-once guard survives `--fast`: a module
the startup hook runs keeps its guard, while the initializer of a program, and
that of a module only the call chain reaches, drop it, and their bodies are
then the initialization statements themselves. The guard at the top of a
procedure or block body is kept in every mode: that one is the save attribute
of an initialized local, so it decides what the program does rather than
repeating a call that cannot happen.

The same hook creates the storage the layout of the module's variables does
not hold in place — a descriptor for an array component, a buffer for a
character component, a type pointer — before it calls the initializer, since
an initialization can use such a component. It stores no value at all: static
data holds every default a constant describes, and the initializer holds
every other, as the `global_init` pass gave it. A component whose initial
state is static data — a constant default at any depth, a `=> null()`
pointer, and a component without a default, such as an integer, which
Fortran leaves undefined — is therefore not written at startup, so startup
code that ran earlier and changed it keeps the change. What the hook creates
and what the initializer assigns is materialized at startup, and so is
order-sensitive in the way the rules above describe: a constructor of another
object file that runs first must not use it. That covers the storage created
at run time and the defaults held in it, a pointer component's initial
procedure or target, and every element of an array of a derived type.

When a main program is there the call chain still runs, before the program's
first statement and in dependency order, every initializer — including those
left out of the startup hook, which is why leaving them out costs nothing
whenever a Fortran main program exists at all.

The LLVM backend leaves out one further case, as a limit of that backend
rather than a fact about the ASR: a module that declares an array pointer or
an allocatable array, because the companion descriptor of such a variable is
a frame slot of `main`, and the initializer has to run after it is filled in.
Such a module is left to the Program as a whole, as before the startup hook
existed, so none of its variables is set up when no Fortran main program is
linked or the program cannot observe the module: adding an unrelated
`integer, allocatable :: a(:)` to a module takes all of the module out of the
startup hook.

A build with `--detect-leaks` frees what a module's storage owns before the
leak report counts. The free belongs to the object file that defines the
module, like the setup, but it cannot be a destructor: the report runs while
`main` is still on the stack, and a destructor runs after it returns. A
constructor of that object file registers the teardown with the runtime
instead, and only when the module owns something to free; the report runs
every registered teardown, in the order they were registered, before it
counts.

#### Saved coarrays

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

#### Materialization policy

Choosing between static data and startup code is a trade-off, and static data
is not always the better side of it. Consider

```fortran
integer, parameter :: n = 100000000
real, parameter :: x(n) = 3
real, save :: y(n) = 4
```

With a four-byte default `real`, `y` laid out as static data takes around
400 MB in the object file and in the executable, and more than that in the
compiler's memory while the constant is built; `x` does the same wherever it
needs storage. A broadcast is compact, one value and a shape, so the intended
design keeps it symbolic throughout compilation — as the `ArrayBroadcast`
node ASR already has, which is what a derived-type broadcast reaches
`global_init` as today — and never expands its elements before the lowering
has been chosen. Materialized at startup instead, `y` takes zero-filled
storage (BSS on most targets, which costs nothing in the file) and a loop
that fills it once, which is a few instructions whatever `n` is — a valid
choice even for a value static data could describe.

Neither side is free. Static zeros are already compact, since they go to BSS.
A smaller file usually helps, but a fill at startup is not always faster: it
writes every page at startup and so turns all of them into private dirty
memory, where static data is paged in from the file on demand and can be
shared between processes. And no strategy makes an arbitrary `n` fit into the
memory or the address space.

The intended design is a materialization policy the user can select — static,
runtime, or automatic, where automatic decides from the size in bytes and the
target. It is a decision of the ASR lowering, and `global_init` is where it
naturally belongs, since that pass already decides which initializers become
statements; a backend then lowers what it is given mechanically. None of it
is implemented yet: there is no option and no threshold, semantics currently
folds an intrinsic-type broadcast such as the two above into an
`ArrayConstant` holding every element, and the LLVM backend lays that out as
static data. What `global_init` decides today is a fixed rule instead: a
declaration initializer stays static data wherever the current layout can
hold it, a scalar module variable's default initialization is static data
except for what is held in storage created at run time, and a module array
of a derived type gets its elements' defaults from a loop at startup, since
its zeros cost nothing in the file however large it is.

A `parameter` stays a compile-time constant under any policy: its definition
stays compact and its Fortran meaning immutable. A use of it is folded without
storage wherever possible. Where a `parameter` array needs storage and the
policy fills it at startup, what is filled is storage the compiler generates
for it: nothing assigns to the user's `parameter`, and the `parameter` keeps
its constant expression.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/program.asr
:language: clojure
```

## See Also

[TranslationUnit](../unit_nodes/TranslationUnit.md), [Module](Module.md), [Function](Function.md), [Variable](Variable.md)
