# Abstract Semantic Representation (ASR)

The aim of ASR is to represent all semantics in a non-redundant way, and that
has all the semantic information available locally, so that the backend can
do a single pass over ASR and have all the information at hand to generate
code.

ASR is always semantically valid Fortran code. It is as far from the original
Fortran language code as possible (i.e. everything is explicitly figured out,
all semantic information gathered and readily available locally from each ASR
node), while ensuring no semantic information was lost (no lowering was
done), so one can still generate Fortran code from ASR that will be logically
equivalent to the original code.

ASR can be used to do Fortran-level transformations (such as optimizations).

For the lossless direct representation used by compiler tests and tools, see
the [ASR text format](asr_text.md).

## The node reference

Every constructor of [ASR.asdl](https://github.com/lfortran/lfortran/blob/main/src/libasr/ASR.asdl)
has a page of its own, grouped by the type it belongs to:

- [unit](asr_nodes/unit_nodes/unit_nodes.md) — the root of the graph;
- [symbol](asr_nodes/symbol_nodes/symbol_nodes.md) — what a symbol table maps
  a name to;
- [stmt](asr_nodes/statement_nodes/statement_nodes.md) — executed for effect;
- [expr](asr_nodes/expression_nodes/expression_nodes.md) — has a type and
  produces a value;
- [ttype](asr_nodes/type_nodes/ttype.md) — what a value is;
- [helper nodes](asr_nodes/helper_nodes/helper_nodes.md) — the products and
  small sum types that appear inside the nodes;
- [enumerations](asr_nodes/enum_nodes/enum_nodes.md),
  [cast_kind](asr_nodes/cast_kind_nodes/cast_kind.md) and
  [kinds](asr_nodes/kinds_nodes/kinds.md);
- [OpenMP nodes](asr_nodes/omp_nodes/omp_nodes.md).

Each page shows the declaration exactly as `ASR.asdl` spells it, describes
every member, and includes a complete ASR text example under
`doc/src/asr/examples/`. `tests/asr/check_docs.py` checks that every example
round-trips through the ASR text reader, that every excerpt is a verbatim part
of the example it is taken from, and that every declaration on a page still
matches `ASR.asdl`, so a change to the ASR that the documentation does not
follow fails a test rather than going unnoticed.

## Abstract Syntax Description Language (ASDL)

Abstract Syntax Description Language describes the abstract syntax of the compiler
IRs and other tree-like data structures. IRs described with ASDL are converted
into an implementation automatically by tools. Tools generate the data structure
definitions for a target language, pickling functions, and other codes.

ASDL consists of three fundamental constructs: ***types, constructors***, and
***productions***.

Let's take an example of a node from [ASR.asdl](https://github.com/lfortran/lfortran/blob/main/src/libasr/ASR.asdl):

```text
Program(symbol_table symtab, identifier name, identifier* dependencies, stmt* body, location start_name, location end_name)
```

### Types
The **types** are required to begin with a lowercase. ASDL's builtin
types are:
- identifier
- int (signed integers of infinite precision)
- string

We extend these by:
- bool (.true. / .false.)
- float (floating point number of infinite precision)
- symbol_table (scoped Symbol Table implementation)
- node (any ASR node)
- location (a span of the original source, used for diagnostics; location
  members are not printed in the ASR text format)

> ***Note***: symbol_table contains `identifier` -> `symbol` mappings

In the above example, `symbol_table`, `identifier`, `stmt`, and `location` are
types.

### Constructors
The **constructors** names must begin with an upper case. The `symbol` type has
`Program`, `Module`, `Function` and the other constructors listed on the
[symbol](asr_nodes/symbol_nodes/symbol.md) page, where the `Program`
constructor has the four members above plus the two source locations. These
are, basically, subtrees.

## Symbol type

Each symbol has either `symtab` (local symbol table) or `parent_symtab`
(where this symbol is stored). One can get to parent_symtab via symtab, so
only one is present.

Each symbol has a `name` for easy lookup of the name of the symbol when only
having a pointer to it.

`abi=Source` means the symbol's implementation is included (full ASR),
otherwise, it is external (interface ASR, such as procedure interface).

`SubroutineCall`/`FunctionCall` stores the actual final resolved subroutine or
function (`name` member). They also store the original symbol
(`original_name`), which can be one of: `null`, `GenericProcedure` or
`ExternalSymbol`.

### Call argument intent contract

For call nodes (`SubroutineCall` / `FunctionCall`), actual argument expressions
must satisfy the dummy argument intent:

- `intent(in)`: any expression is allowed.
- `intent(out)` / `intent(inout)`: actual argument must be writable (a variable
  expression, or a cast wrapper such as `Cast`, `ArrayPhysicalCast`,
  `StringPhysicalCast` around writable storage).
- `intent(unspecified)`: any expression is allowed at the call site. Whether
  the callee writes through the argument is runtime-dependent.

When a module is compiled, it is parsed into full ASR, an object file is
produced, and the full ASR (abi=Source, "body" is non-empty) is transformed into
interface ASR (abi=LFortran, "body" is empty). Both interface and full ASR
are saved into the mod file.

When a module is used, it is first looked up in the symbol table (as either
full or interface ASR) and used if it is present. Otherwise, a mod file is
found on the disk, loaded (as either full or interface ASR for LFortran's
mod file, depending on LFortran's compiler options; or for GFortran's mod
file, the corresponding interface ASR is constructed with abi=GFortran) and
used. After the ASR is loaded, the symbols that are used are represented as
ExternalSymbols in the current scope of the symbol table.

ExternalSymbol represents symbols that cannot be looked up in the current
scoped symbol table. As an example, if a variable is defined in a module,
but used in a nested subroutine, that is not an external symbol
because it can be resolved in the current symbol table (nested subroutine)
by following the parents. However, if a symbol is used from a different
module, then it is an external symbol because the usual symbol resolution by
going to the parents will not find the definition. The `module_name` member
is the name of the module the symbol is in, and the `scope_names` is a list of
names if the symbol is in a nested symbol table. For example, if it is a
local variable in a function `f` that is nested in function `g`, then
`scope_names=[g, f]`.

REPL: each cell is parsed into full ASR, compiled + executed, and the full ASR
is transformed into interface ASR (abi=LFortran) and kept in the symbol
table. A new cell starts with an empty symbol table, whose parent symbol
table is the previous cell. That allows function/declaration shadowing.

## ABI Type

The [abi](asr_nodes/enum_nodes/abi.md) member of a symbol says where its
implementation lives and which calling convention reaches it:

|                    | External | ABI          |
|--------------------|----------|--------------|
| `Source`           | No       | Unspecified  |
| `LFortranModule`   | Yes      | LFortran     |
| `GFortranModule`   | Yes      | GFortran     |
| `BindC`            | Yes      | C            |
| `BindPython`       | Yes      | Python       |
| `BindJS`           | Yes      | JavaScript   |
| `ExternalUndefined`| Yes      | Unspecified  |
| `Intrinsic`        | Yes      | Unspecified  |

- **External Yes**: the symbol's implementation is not part of ASR; the
symbol is just an interface (e.g., subroutine/function interface, or variable
marked as external, not allocated by this ASR).
- **External No**:  the symbol's implementation is part of ASR (e.g.,
subroutine/function body is included, variables must be allocated).
- **abi=Source**: The symbol's implementation is included in ASR, and the backend is
free to use any ABI it wants (it might also decide to inline or eliminate
the code in optimizations).
- **abi=LFortranModule/GFortranModule/BindC**: the symbol's implementation is
stored as machine code in some object file that must be linked in. It
uses the specified ABI (one of the LFortran modules, GFortran module, or C ABI).
An interface that uses `iso_c_binding` and `bind(c)` is represented using
abi=BindC.
- **abi=BindPython**: the symbol's implementation is
stored in text format in the user source code file.
The symbol is executed using the CPython interpreter.
LPython manages the conversion of arguments to be passed to such symbols
and also converts the return values from such symbols.
- **abi=BindJS**: the symbol's implementation is
available with Javascript.
This abi type is to be mainly used with the WASM Backend.
- **abi=ExternalUndefined**: the symbol is external and no calling convention
has been recorded for it yet.
- **abi=Intrinsic**: the symbol's implementation is implicitly provided by the
language itself as an intrinsic function. That means the backend is free to
implement it in any way it wants. The function does not have a body, it is
just an interface.

## Short notes on ASR nodes

### Stmt nodes
1. **ExplicitDeallocate**: It deallocates if allocated otherwise throws a runtime error.
2. **ImplicitDeallocate**: It  deallocates if allocated otherwise does nothing.
3. **GoTo**: It points to a GoToTarget with the corresponding target_id within
the same procedure. We currently use `int` IDs to link GoTo with
GoToTarget to avoid issues with serialization.
4. **GoToTarget**: An empty statement, a target of zero or more GoTo statements
the `id` is only unique within a procedure.

### Expr nodes
1. **Cast**: It changes the value (the bits) of the `arg`.
2. **ArrayPhysicalCast**: only the physical type changes; the logical type
does not.
    > Note: the "new" physical type here will also be part of the "type" member

    This allows to represent any combination, but we'll only support a few; at least we need:
    - Descriptor -> Pointer
    - Pointer -> Descriptor
    - CompileTimeFixedSizeArray -> Pointer
    - CompileTimeFixedSizeArray -> Descriptor
    - Descriptor -> NumPy
    - NumPy -> Descriptor
    - ISODescriptor -> Descriptor
    - Descriptor -> ISODescriptor
3. **StringPhysicalCast**: the same idea for strings, between a descriptor and
a bare `char*`.

### Ttype nodes

See [ttype](asr_nodes/type_nodes/ttype.md) for the whole type language, and
[kinds](asr_nodes/kinds_nodes/kinds.md) for the kinds each intrinsic type
supports.

The length of a [String](asr_nodes/type_nodes/String.md) is described by two
members together: `len_kind` says how the length is determined and `len`
carries the expression when there is one. A `character(*)` dummy argument is
`AssumedLength` with no `len`, a `character(:), allocatable` is
`DeferredLength`, and `character(n+3)` is `ExpressionLength` with the
expression in `len`.

### String format kind

See [string_format_kind](asr_nodes/enum_nodes/string_format_kind.md) for the
formatting languages a [StringFormat](asr_nodes/expression_nodes/StringFormat.md)
can use.
