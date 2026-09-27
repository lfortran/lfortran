# LFortran Design

## High Level Overview

LFortran is structured around two independent modules, AST and ASR, both of
which are standalone (completely independent of the rest of LFortran) and users
are encouraged to use them independently for other applications and build tools
on top:

* Abstract Syntax Tree (AST), module `lfortran.ast`: Represents any Fortran
  source code, strictly based on syntax, no semantic is included. The AST
  module can convert itself to Fortran source code.

* Abstract Semantic Representation (ASR), module `lfortran.asr`: Represents a valid
  Fortran source code, all semantic is included. Invalid Fortran code is not
  allowed (an error will be given). The ASR module can convert itself to an
  AST.

The LFortran compiler is then composed of the following independent stages:

* Parsing: converts Fortran source code to an AST
* Semantic: converts an AST to an ASR
* High level optimizations: optimize ASR to a possibly faster/simpler ASR
  (things like inlining functions, eliminating redundant expressions or
  statements, etc.)
* LLVM IR code generation and lower level optimizations: converts an ASR to an
  LLVM IR. This stage also does all other optimizations that do not produce an
  ASR, but still make sense to do before passing to LLVM IR.
* Machine code generation: LLVM then does all its optimizations and generates
  machine code (such as a binary executable, a library, an object file, or it
  is loaded and executed using JIT as part of the interactive LFortran session
  or in a Jupyter kernel).

LFortran is structured as a library, and so one can for example use the parser
to obtain an AST and do something with it, or one can then use the semantic
analyzer to obtain ASR and do something with it. One can generate the ASR
directly (e.g., from SymPy) and then either convert to AST and to a Fortran
source code, or use LFortran to compile it to machine code directly. In other
words, one can use LFortran to easily convert between the three equivalent
representations:

* Fortran source code
* Abstract Syntax Tree (AST)
* Abstract Semantic Representation (ASR)

They are all equivalent in the following sense:

* Any ASR can always be converted to an equivalent AST
* Any AST can always be converted to an equivalent Fortran source code
* Any Fortran source code can always be either converted to an equivalent AST
  or one gets a syntax error
* Any AST can always be either converted to an equivalent ASR or one gets a
  semantic error

So when a conversion can be done, they are equivalent, and the conversion can
always be done unless the code is invalid.

## ASR Design Details

The ASR is designed to have the following features:

* ASR is still semantically equivalent to the original Fortran code (it did not
  lose any semantic information). ASR can be converted to AST, and AST to
  Fortran source code which is functionally equivalent to the original.

* ASR is as simple as possible: it does not contain any information that could
  not be inferred from ASR.

* The ASR C++ classes (down the road) are designed similarly to SymEngine: they
  are constructed once and after that they are immutable. The constructor
  checks in Debug more that all the requirements are met (e.g., that all
  Variables in a Function have a dummy argument set, that explicit-shape arrays
  are not allocatable and all other Fortran requirements to make it a valid
  code), but in Release mode it quickly constructs the class without checks.
  Then there are builder classes that construct the ASR C++ classes to meet
  requirements (checked in Debug mode) and the builder gives an error message
  if a code is not a valid Fortran code, and if it doesn't give an error
  message, then the ASR C++ classes are constructed correctly. Thus by
  construction, the ASR classes always contain valid Fortran code and the rest
  of LFortran can depend on it.

## Compilation Modes

We support two compilation modes:

1. Monolithic Compilation Mode: The standard compilation mode. 
In this mode, LFortran produces empty object files (`.o` files) with `-c` flag (the only reason to produce those is to satisfy existing build systems that typically expect `.o` files to be created). The object code is generated only when main program is encountered: all modules are loaded from `.mod` files and everything compiled and linked at once. When a module is compiled, only a `.mod` file is generated with full code. For files with global procedures, LFortran identifies those automatically and sets `generate_code_for_global_procedures` compiler option to `true` (not exposed to user), which then generates object code only for global procedures (you must thus link these generated `.o` object files with the main program), and rest of the modules are serialized to `.mod` files which do not contain global procedure.

2. Separate Compilation Mode: This mode is enabled with `--separate-compilation` flag. In this mode, LFortran generates full code for each file into object files (`.o` files) with full symbol information. This
is usually the default mode used by most other Fortran compilers. We create object code, and we still create `.mod` files for modules and they contain everything just like for direct mode but when any `.mod` file is loaded, we change all symbols to `ExternalUndefined` ABI. We don't change the ABI for `bind(c)` (since those are undefined already in object code, the user is responsible to provide an implementation at link time).

Note: **_If you enable separate compilation mode, you have to enable it for all the files._**

## Initialization of Variables

LFortran keeps three parts of initializing a variable apart: the initial state
the language defines, which ASR states; the physical setup a backend's
representation of the storage needs, such as array descriptors and character
buffers; and the choice of materializing an initial value as static data or by
code that runs once at startup. That choice is made in the ASR lowering, by
the `global_init` pass, which turns each declaration initializer static data
does not hold, and each default of a module variable it does not hold, into a
statement of a startup initializer. A backend only carries the choice out: it
lays the rest out as static data, and creates the storage the layout needs,
without storing a value, where the initializer's `GlobalInitStorage` says.
Today the choice follows a fixed rule; a policy the user can select, for large
arrays in particular, is the intended design but not implemented. Every
variable has to be initialized exactly once, before anything can observe it,
whether a Fortran main program, a C `main` or a library user drives the code
and whatever the compilation mode. Startup code never stores back a value that
static data already holds, and initialization that depends on other code
having run is ordered by explicit calls in ASR rather than by the order in
which a linker runs constructors: every initializer is guarded, calls the
initializers it depends on first, and one startup engine, entered from
constructors, main programs and foreign entry points alike, runs them all in a
stable order. The *Startup initializers* section of
[Program](asr/asr_nodes/symbol_nodes/Program.md) has the details, including
what is lowered to startup code today and the intended materialization policy.

### Startup in each backend

Every backend consumes the same ASR: the owner-linked initializers with their
guards, `GlobalInitStorage` and `GlobalInitDispatch`. What differs is how the
set of initializers to run is found.

* **LLVM, C and C++** produce object files that are linked with other code, so
  the set is open. Each object file carries a static table of records for the
  initializers it defines, in the encoding of its object format, a constructor
  that enters the runtime engine, which finds the tables of every loaded image
  by itself, and a destructor that takes its records back out before the image
  is unloaded. The table and its records are laid out as
  `runtime/lcompilers_init_abi.h` describes (ABI version 3): each record names
  an initializer, its state and teardown, and says whether it runs in the
  local phase, in the collective phase, or is a collective bootstrap, which
  initializes the coarray runtime once, outside every guard, before any
  collective initializer. A table a loader lists also points to a private
  writable word of its object, through which the engine tells one mapping of
  an image from a later one loaded at the same address. WebAssembly has no
  loader to ask, so there each object instead publishes its table from a
  constructor that runs before any other. The constructor and destructor pass
  the table itself on every format, which alone registers it from the
  constructor on; the encoding of the object format only makes it known
  before any constructor runs. The LLVM backend emits the table, its
  constructor and destructor independent of the object format, which is what
  `--show-llvm` prints and which runs as it is when another tool compiles it,
  and adds the encoding of the target's object format only when the module
  is lowered to an object file or assembly (`lower_global_init_records`).
  The C and C++ backends emit the same
  records with compiler attributes (the ELF note, whose descriptor is an
  offset only the linker resolves, with assembler directives), or with
  MSVC's section pragmas when MSVC compiles them. They lower `GlobalInitStorage` to nothing only for a variable
  whose declaration already is all of its storage; storage these backends
  would have to create at run time (module arrays, allocatables, C++ objects)
  is reported as not supported, as such module variables were not supported by
  them before either.
* **Direct WebAssembly, MLIR and Fortran source** are closed worlds: the
  output is the whole program, or is compiled on its own without the
  LCompilers runtime. `ASRUtils::expand_closed_world_dispatch` replaces each
  `GlobalInitDispatch` by calls of the initializers in stable id order (the
  local ones, then the collective bootstraps, then the collective ones) and
  each guard by plain code on its state, so nothing of the runtime engine is
  left. Fortran source (`--show-fortran`, `--backend=fortran`) therefore needs
  no runtime to start up, and the names the passes created that are not
  Fortran names, such as those starting with an underscore or longer than 63
  characters, are renamed the way `--apply-fortran-mangling` renames them: a
  name that is too long keeps its start and ends in a hash of all of it, so
  every file printed separately agrees on it. A dump of the passes
  (`--dump-all-passes-fortran`) shows the open-world form instead, with
  explicit `bind(c)` interfaces to the engine. A `bind(c)` procedure's
  dispatch becomes calls of every initializer that needs no collective
  boundary as well, and one whose declarations read startup state is an entry
  that makes those calls before it calls the implementation holding the
  declarations, so a foreign caller that enters before the main program, from
  a C constructor for instance, finds the state initialized. Only the
  collective initializers, those of saved coarrays, wait for the main program.
  Direct WebAssembly and MLIR report an error for a module compiled
  separately, whose initializer the output could not contain, and direct
  WebAssembly, only entered through its main program, also for modules
  compiled without one. Fortran source keeps calling the initializer of a
  module compiled separately, which that module's own source defines.
* **x86** has no module variables and runs none of these passes.

In interactive mode each cell is compiled into code the JIT holds in memory,
which no loader discovers and whose constructors the JIT does not run. A cell
that defines initializers gets an entry, `<run function>_startup`, that hands
its whole table of records to the engine and dispatches; the evaluator calls
it right after adding the cell and before running anything of it, so a cell
that only defines a module initializes it there. The dispatch is collective
when one of the cell's initializers needs a collective boundary, as those of
saved coarrays do. Definitions of earlier cells stay initialized; a module a
later cell redefines is a new definition and is initialized anew. The matching
`<run function>_shutdown` tears down what the cell's initializers set up,
with or without leak detection, and takes the table back out; the evaluator
calls those, newest first, before the JIT's code goes away.

### Linking a coarray program

A program that uses coarrays is linked with a PRIF implementation, such as
Caffeine, and with the adapter through which LFortran's startup starts it,
`lcompilers_prif.f90`, installed in `share/lfortran/prif/`. The adapter has to
be compiled with that implementation's own `prif` module, since only it knows
the status the implementation gives when it was started already, and by a
compiler that reads that module, with the implementation's module ABI. With
LFortran, `--separate-compilation` keeps the object from defining anything of
the implementation's module again:

```console
$ lfortran -c --separate-compilation \
      -I<directory of the implementation's prif.mod> \
      lcompilers_prif.f90 -o lcompilers_prif.o
$ lfortran --coarray <objects> lcompilers_prif.o -L<caffeine>/lib -lcaffeine \
      -lgasnet-smp-seq
```

The object is compiled once and goes explicitly on the link line of every
coarray program, next to the implementation's libraries; `--coarray` does not
add it. Its `bind(c)` entry initializes the `prif` module it uses before it
calls `prif_init`. A host that starts the runtime itself and calls
`lcompilers_initialize()` needs no other arrangement: a runtime that is
already started is not an error.

## Notes:

Information that is lost when parsing source to AST:
whitespace, multiline/single line if statement distinction, case sensitivity of keywords.

Information that is lost when going from AST to ASR:
detailed syntax how variables were defined and the order of type attributes (whether array dimension is using the `dimension` attribute, or parentheses at the variable; or how many variables there are per declaration line or their order), as ASR only represents the aggregated type information in the symbol table.

ASR is the simplest way to generate Fortran code, as one does not
have to worry about the detailed syntax (as in AST) about how and where
things are declared. One specifies the symbol table for a module, then for
each symbol (functions, global variables, types, ...) one specifies the local
variables and if this is an interface then one needs to specify where one can
find an implementation, otherwise a body is supplied with statements, those
nodes are almost the same as in AST, except that each variable is just a
reference to a symbol in the symbol table (so by construction one cannot have
undefined variables). The symbol table for each node such as Function or Module
also references its parent (for example a function references a module,
a module references the global scope).

The ASR can be directly converted to an AST without gathering any other
information. And the AST directly to Fortran source code.

The ASR is always representing a semantically valid Fortran code.  This is
enforced by checks in the ASR C++ constructors (in Debug build).
When an ASR is used, one can assume it is valid.

## Conditional Expressions and Conditional Arguments

A conditional expression (F2023 10.1.2.3), `( cond ? a : b )`, is an ordinary
expression and becomes the `ConditionalExpr` AST node. A conditional argument
(15.5.1) has the same syntax but appears in an actual argument position, where
it selects the actual argument itself rather than a value: a consequent that is
a variable stays definable, and a consequent of `.NIL.` (6.2.1) means the dummy
argument is not present.

Only the context tells the two apart (C1535), so the parser does not. `.NIL.` is
a token of its own (6.2.1) rather than a named constant or a defined operator,
matched ahead of the defined-operator rule in the tokenizer, and a single
`cond_consequent` production — an expression, a variable or `.NIL.` — is shared
by both places a conditional expression is parsed. `.NIL.` therefore reaches the
AST as the `Nil` expression node from any consequent position.

The semantic layer then rejects `Nil` wherever a value is required, and expands a
conditional argument by duplicating the procedure reference into the arms of a
selection, one copy per consequent, so that every copy is an ordinary reference
that the existing argument checks apply to.

## Fortran 2008

Fortran 2008 [standard](https://j3-fortran.org/doc/year/10/10-007.pdf) chapter
2 "Fortran concepts" specifies that Fortran code is a collection of _program
units_ (either all in one file, or in separate files), where each _program
unit_ is one of:

* main program
* module or submodule
* function or subroutine

Note: It can also be a _block data_ program unit, that is used to provide
initial values for data objects in named _common blocks_, but we do not
recommend the use of _common blocks_ (use modules instead).

## LFortran Extension

We extend the Fortran language by introducing a _global scope_, which is not
only the list of _program units_ (as in F2008) but can also include statements,
declarations, use statements and expressions. We define _global scope_ as a
collection of the following items:

* main program
* module or submodule
* function or subroutine
* use statement
* declaration
* statement
* expression

In addition, if a variable is not defined in an assignment statement (such as
`x = 5+3`) then the type of the variable is inferred from the right hand side
(e.g., `x` in `x = 5+3` would be of type `integer`, and `y` in `y = 5._dp`
would be of type `real(dp)`). This rule only applies at the top level of
_global scope_. Types must be fully specified inside main programs, modules,
functions and subroutines, just like in F2008.

The _global scope_ has its own symbol table. The main program and
module/submodule do not see any symbols from this symbol table. But functions,
subroutines, statements and expressions at the top level of _global scope_ use
and operate on this symbol table.

The _global scope_ has the following symbols predefined in the symbol table:

* the usual standard set of Fortran functions (such as `size`,
  `sin`, `cos`, ...)
* the `dp` double precision symbol, so that one can use `5._dp` for double
  precision.

Each item in the _global scope_ is interpreted as follows: main program is
compiled into an executable with the same name and executed; modules,
functions and subroutines are compiled and loaded; use statement and
declaration adds those symbols with the proper type into the _global scope_
symbol table, but do not generate any code; statement is wrapped into an
anonymous subroutine with no arguments, compiled, loaded and executed;
expression is wrapped into an anonymous function with no arguments returning
the expression, compiled, loaded, executed and the return value is returned
to the user.

The _global scope_ is always interpreted, item by item, per the previous
paragraph. It is meant to allow interactive usage, experimentations and
writing simple scripts. Code in _global scope_ must be interpreted using
`lfortran`. For more complex (production) code it is recommended to turn it
into modules and programs (by wrapping loose statements into subroutines or
functions and by adding type declarations) and compile it with `lfortran` or
any other Fortran compiler.

Here are some examples of valid code in _global scope_:

### Example 1

```fortran
a = 5
print *, a
```

### Example 2

```fortran
a = 5

subroutine p()
print *, a
end subroutine

call p()
```

### Example 3

```fortran
module a
implicit none
integer :: i
end module

use a, only: i
i = 5
```

### Example 4

```fortran
x = [1, 2, 3]
y = [1, 2, 1]
call plot(x, y, "o-")
```

## Design Considerations

The LFortran extension of Fortran was chosen in a way so as to minimize the
number of changes. In particular, only the top level of the _global scope_
has relaxed some of the Fortran rules (such as making specifying types
optional) so as to allow simple and quick interactive usage, but inside
functions, subroutines, modules or programs this relaxation does not apply.

The number of changes were kept to minimum in order to make it
straightforward to turn code at _global scope_ into standard compliant
Fortran code using programs and modules, so that it can be compiled by any
Fortran compiler.
