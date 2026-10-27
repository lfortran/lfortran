# LFortran Design

The native prompt and optional Xeus kernel share an
[interactive evaluation and runtime input API](interactive_evaluation.md).

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
Submodules are the other exception: the object file of a file that contains a submodule (and no main program) has the code of that submodule, its separate module procedures and its own variables and procedures, and nothing else of the modules it uses; you must thus link it with the main program as well. A program, or any other file, that loads a submodule from its `.smod` file only declares what the submodule defines. The code of a submodule is therefore in the object file of its own source, so that the program can be compiled before or after the submodule, and recompiling only a changed submodule and linking again runs its new code. The parent module and the modules it uses are still emitted with the main program, which keeps all of their procedures and gives their variables external linkage, as the submodule's object file refers to them. A module that only submodules use is not part of this exception: it is emitted with the main program only if the program loads a submodule that uses it, that is, only if the submodule was compiled before the program.

2. Separate Compilation Mode: This mode is enabled with `--separate-compilation` flag. In this mode, LFortran generates full code for each file into object files (`.o` files) with full symbol information. This
is usually the default mode used by most other Fortran compilers. We create object code, and we still create `.mod` files for modules and they contain everything just like for direct mode but when any `.mod` file is loaded, we change all symbols to `ExternalUndefined` ABI. We don't change the ABI for `bind(c)` (since those are undefined already in object code, the user is responsible to provide an implementation at link time).

Note: **_If you enable separate compilation mode, you have to enable it for all the files._**

## COMMON Blocks

A COMMON block is storage that every scoping unit declaring it shares, while
each unit gives it a layout of its own. The ASR keeps that layout with the
unit: a scoping unit that declares a block owns a `CommonBlock` symbol, its
view of the block, which lists the unit's own variables in order (`members`)
and places the variables that EQUIVALENCE connects to the block
(`associations`). In interactive use, a block declared at the top level has
its view in the global scope. Each of those variables points back to the view through
`Variable.common_block`, and a `Var` refers to it like to any other variable.
A module variable in a COMMON block is an ordinary module variable for the
units that use the module: it is imported through an `ExternalSymbol`, and its
view is stored in the module file together with it.

Views are connected only through `link_name`, the global symbol of the
storage, which is the same in every view of a block. Nothing matches the
variables of two views by name. The views in the ASR are the only persistent
record of a block: the frontend makes up no module, derived type or module file
for it, and keeps no side table of layouts across scoping units. A backend
derives the layout of each `link_name` locally while it emits a translation
unit: it combines the views of one `link_name` into a single definition and
addresses every variable at the position its own view gives it. On targets with
common symbols, the linker combines the definitions of different object files.
The LLVM WebAssembly target has no common symbols and no way to size a block
at run time, so it supports a program lowered as one translation unit:
definitions of one block in separate objects conflict at link time.

The `role` of a view says what its definition is. A view whose variables have
initializers is `Initialized`, even if every value is zero. These are the views
of a `BlockData` unit: the unit has no statements, and its DATA values are the
initializers of its own variables, so its object file defines the storage with
those values, in both compilation modes. A view without initial values is
`Tentative`, or `External` when the storage is defined elsewhere: by the
object file of a module read from its module file, or by an earlier
interactive cell, after which the block cannot grow or be given initial
values. See [CommonBlock](asr/asr_nodes/symbol_nodes/CommonBlock.md).

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
