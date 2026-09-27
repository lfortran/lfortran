# Program

The main program: the entry point of an executable.

## Declaration

### Syntax

```text
Program(symbol_table symtab, identifier name, identifier* dependencies,
    stmt* body, symbol? global_init, symbol? global_init_state,
    location start_name, location end_name)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the symbol table of the program. It owns the local variables of the program and the `ExternalSymbol` entries for the module symbols the program uses. |
| `name` | the name of the program. |
| `dependencies` | the names of the modules and procedures the body of the program refers to. The backends use it to order code generation. |
| `body` | the statements of the program, in order. |
| `global_init` | this program's own initializer, a symbol of `symtab`, or `nil`: what the program's frame needs set up before its first statement. See the description below. |
| `global_init_state` | the saved `integer(4)` variable of `symtab` that guards `global_init`, or `nil` exactly when `global_init` is. |
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
that starts the runtime and calls `bind(c)` procedures, LFortran's output
used as a library, separately compiled object files — every variable has to
be initialized exactly once, before anything can observe it. Three rules
follow from that:

* Code that runs at startup does only what the static data does not already
  hold. It never stores a static value back: static data is in place before
  any code runs, while startup code can run after user or library code has
  already changed the storage, and storing the initial value again would undo
  that change.
* ASR states the initial state; a target only carries it out. Everything a
  target does at startup is stated in ASR: the startup initializers and the
  calls between them, [GlobalInitStorage](../statement_nodes/GlobalInitStorage.md)
  for the physical setup, and
  [GlobalInitDispatch](../statement_nodes/GlobalInitDispatch.md) for where the
  startup engine is entered. A target never works out what to initialize, or
  what to leave alone, by inspecting the instructions it generated.
* Initialization that depends on other code having run first is ordered by
  explicit calls in ASR, not by the order in which a linker happens to run the
  constructors of different object files, which cannot be predicted from the
  source.

#### What is executable today

The `global_init` ASR pass moves a declaration initializer that the current
lowering does not materialize as static data out of the declaration and into
a **startup initializer**: an argument-less procedure in the owner's own
symbol table, which the owner's `global_init` refers to. A [Module](Module.md)
and a **Program** each have at most one, which is why the link lives on the owner
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
t(null())` already gets. A test that exercises one of these through startup
code tests the startup code, not the impossibility of static data.

A procedure or a block needs no initializer procedure of its own: nothing
outside it can observe its variables, so there is nobody to call one. The same
pass puts their initialization straight into a guarded block at the top of
their own body, which is what the save attribute Fortran gives every
initialized local means.

#### The initializers and their guards

Every user [Module](Module.md) and submodule owns a startup initializer from
the moment semantics creates it, referred to by its `global_init`, whatever its
initialization lowers to later: its module file carries it, so every
translation unit that depends on the module calls that one definition, and a
translation unit that only uses a module read from a module file declares the
initializer without defining it. A **Program** and the
[TranslationUnit](../unit_nodes/TranslationUnit.md) get one when a pass has
something to put into it. Each initializer is guarded by the state its owner's
`global_init_state` refers to, and the `global_init_wire` pass, which runs after
every pass that can add initialization (`coarray` among them), gives each
initializer defined in the translation unit its final shape:

```text
if (_lcompilers_init_begin(state) /= 0) then
    call <the initializer of each owner this one depends on, by stable id>
    GlobalInitStorage(<the variables whose storage is created at run time>)
    <the initialization statements>
    call _lcompilers_init_end(state)
end if
```

The state is not initialized, being initialized, or ready, and it becomes
ready only once the whole body has run, physical setup and values together.
`_lcompilers_init_begin` returns 0 once the state is ready; otherwise it
serializes initialization, marks the state as being initialized and returns 1.
Finding the state being initialized by the calling thread is an initialization
cycle: the initialization of a definition depends on itself, which is
reported, and ends the process. A state another thread is initializing is
waited for.

What an initializer depends on is an ordinary call at its top, to the guarded
initializer of each module it depends on: for a module, its parent module, the
modules it uses and the modules its initializer imports from, and for a
program, the modules its initializer imports from; only modules that have an
initializer count, each once, in stable id order. That call runs the
dependency's initialization if it has not run yet and returns at once if it
has, so the order is right whichever initializer is reached first, and no
constructor order or graph at run time is involved.

The guard of initialized locals at the top of a procedure or block body is a
different thing: it is the save attribute Fortran gives every initialized
local, so it is kept in every mode.

#### The startup engine

Each initializer of a module or of the translation unit is a *root* the
runtime engine runs before user code, identified by a stable id derived from
its owner (`m:<module>`, `s:<parent>:<submodule>`, `t:<initializer>`), the
same on every image and in every compilation. A code generator whose output is
linked with other code emits a static table of records for the roots the
translation unit defines, in the encoding of its object format, and a
constructor that enters the engine. The engine finds the tables of every
loaded image itself, so the first constructor that enters it already sees all
of them, and it calls every root in stable id order. Code the loader cannot
see, such as that of an interactive cell, hands its table to the engine
explicitly before any of it runs, and so does every object file of a
WebAssembly image, from a constructor that runs before any other.

`GlobalInitDispatch` is where generated code enters the engine: first thing in
every program, with the collective phase. The program's own initializer is
not a root: it sets up the program's frame, so the program calls it itself,
after the dispatch and before its first statement. No procedure enters the
engine, `bind(c)` procedures included, so calling one costs nothing for the
startup, however often it is called.

A program whose main function is not Fortran, a C driver above all, enters
the same collective boundary through `lfortran_initialize(argc, argv)`,
declared in LFortran's `ISO_Fortran_binding.h`: that is the contract of such
a host. It calls it on every image before it calls any Fortran procedure, and
may call it again, which initializes what images loaded since define and
leaves what is initialized as it is; `lfortran_finalize()` flushes and
closes the units, as the end of a main program does, but does not perform a
coarray image's normal termination, which the host does itself (see
`doc/src/design.md`). A Fortran procedure called before it, from a C
constructor for instance, is outside the contract and may find its module
variables uninitialized, except for what static data holds.

What the engine can see depends on the loader. On ELF (verified with glibc;
musl and the BSD loaders are unverified) and on Mach-O, the first constructor
that enters it sees the tables of every loaded image, constructed or not, and
so does `lfortran_initialize` called from a constructor that runs before the
object files' own. Each object file's constructor enters it too, with the
local phase, so a library loaded with `dlopen` initializes itself when it is
loaded. On Windows a DLL's records are taken in only once that DLL's own
constructor, its CRT attach, has run, and a DLL constructor does not run the
engine under the loader lock; a DLL whose `LoadLibrary` is still in progress,
or fails, is not taken in, so a dispatch does not see the tables of a DLL
that is not constructed yet. This Windows behaviour is compiled but has not
been run. The guarantee is therefore at the boundaries: a Fortran main
program, or a host calling `lfortran_initialize` after loading its
libraries, sees everything attached by then. The bodies of initializers and
teardowns make no calls into the loader; the coarray runtime's own
initialization, which may, runs in the bootstrap, outside every guard.

A target whose output is the whole program, and so has no loader to ask, gets
the same schedule in plain code: `ASRUtils::expand_closed_world_dispatch`
replaces each `GlobalInitDispatch` by calls of every initializer in stable id
order and each guard by code on its state, so nothing of the engine is left.

`GlobalInitStorage` creates the storage the layout of a variable does not
hold in place before any value is stored, and stores no value: static data
holds every default a constant describes, and the initializer's statements
hold every other, as the `global_init` pass gave them. A component whose
initial state is static data is therefore never written at startup, so code
that ran earlier and changed it keeps the change.

A build with `--detect-leaks` frees what a root's storage owns before the
leak report counts: each record can carry a teardown, which the engine runs
for every initializer that became ready, in the reverse of the order they
became ready, and for an image being unloaded before its code goes away.

#### Saved coarrays

Which initializer allocates a saved coarray is not decided by where it is
declared but by which program unit encloses that declaration, because
allocating a coarray is collective and so cannot wait until control first
reaches the procedure that declares it. The `coarray` pass walks outwards from
the declaration to the first enclosing [Module](Module.md) and uses that
module's initializer, so a saved coarray of a module procedure is allocated by
the module's, exactly as if it had been declared in the module. A saved
coarray of a **Program**, or of a procedure inside one, and one of an
*external* procedure, which no program unit encloses, become static storage of
the [TranslationUnit](../unit_nodes/TranslationUnit.md), a pointer with its
handle and data companions, and are allocated by the translation unit's
collective initializer, which is a root like a module's. The program's own
initializer never allocates a coarray: it sets up the program's frame, which
exists only once the program runs.

Every image has to allocate its saved coarrays in the same order, so an
initializer that allocates one first calls
`_lcompilers_init_require_collective`, and its owner's
`global_init_collective` is set, as is that of every owner that depends on it.
Such an initializer runs only in the collective phase of the engine, entered
at a collective boundary every image reaches: the start of a main program, or
a host calling `lfortran_initialize`. There every image runs the same roots
in the same stable id order, and each root makes the same dependency calls in
the same order, so the collective allocations happen in the same sequence on
every image.

The coarray runtime has to be started before the first allocation, and it is
built from Fortran modules with startup state of its own, so starting it
cannot happen inside another initializer's guard. Every translation unit that
calls the coarray runtime therefore has a collective bootstrap, named by its
`global_init_bootstrap`: an ordinary private subroutine without a guard, whose
record carries the bootstrap flag. It calls `prif_init` and treats every
status it returns as a running runtime: 0 when this call started it, and
`PRIF_STAT_ALREADY_INIT` when it was started already, by another bootstrap or
by the host; a runtime that cannot start does not return from `prif_init`.
The engine runs one bootstrap per stable id, outside every guard, after the
local initializers, which initialize the startup state of the PRIF
implementation's own modules, and before any collective initializer. No
initializer and no program calls `prif_init` itself. Every collective
initializer that allocates saved coarrays ends with a `sync all`, so their
initial values are visible on every image once the collective boundary has
run.

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
