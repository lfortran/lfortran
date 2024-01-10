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

## Abstract Syntax Description Language (ASDL)

Abstract Syntax Description Language describes the abstract syntax of the compiler
IRs and other tree-like data structures. IRs described with ASDL are converted
into an implementation automatically by tools. Tools generate the data structure
definitions for a target language, pickling functions, and other codes.

ASDL consists of three fundamental constructs: ***types, constructors***, and
***productions***.

Let's take an example of a node from [ASR.asdl](https://github.com/lfortran/lfortran/blob/main/src/libasr/ASR.asdl):
```asdl
symbol
    = Program(symbol_table symtab, identifier name, identifier* dependencies, stmt* body)
    | Module(symbol_table symtab, identifier name, identifier* dependencies, bool loaded_from_mod, bool intrinsic)
    | Function(symbol_table symtab, identifier name, ttype function_signature, identifier* dependencies, expr* args, stmt* body, expr? return_var, access access, bool deterministic, bool side_effect_free, string? module_file)
```

#### Types
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

> ***Note***: symbol_table contains `identifier` -> `symbol` mappings

In the above example, `symbol_table`, `identifier`, `stmt`, `bool`, etc are types.

#### Constructors
The **constructors** names must begin with an upper case.
In above example has three constructors, `Program`, `Module`, and `Function`,
where the Program constructor has four fields whose values are of type `symbol_table`,
`identifier`, `identifier*`, and `stmt*`. These are, basically, subtrees.

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
```asdl
abi                   -- External     ABI
    = Source          --   No         Unspecified
    | LFortranModule  --   Yes        LFortran
    | GFortranModule  --   Yes        GFortran
    | BindC           --   Yes        C
    | BindPython      --   Yes        Python
    | BindJS          --   Yes        Javascript
    | Interactive     --   Yes        Unspecified
    | Intrinsic       --   Yes        Unspecified
```
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
- **abi=Interactive**: the symbol's implementation has been provided by the
previous REPL execution (e.g., if LLVM backend is used for the interactive
mode, the previous execution generated machine code for this symbol's
implementation that was loaded into memory). Note: this option might be
converted/eliminated to just use LFortran ABI in the future.
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
2. **ArrayPhysicalCast**: This ArrayPhysicalCast we only change the physical type,
the logical type does not change
    > Note: the "new" physical type here will also be part of the "type" member

    This allows to represent any combination, but we'll only support a few; at least we need:
    Maybe it's easier to add an enumeration here:
    - Descriptor -> Pointer
    - Pointer -> Descriptor
    - CompileTimeFixedSizeArray -> Pointer
    - CompileTimeFixedSizeArray -> Descriptor
    - Descriptor -> NumPy
    - NumPy -> Descriptor
    - ISODescriptor -> Descriptor
    - Descriptor -> ISODescriptor

### Ttype nodes
```asdl
ttype = Integer(int kind) | UnsignedInteger(int kind) | Real(int kind) | ...
```

**`len`** in Character:
- $>=0$ ... the length of the string, known at compile time
- $-1$ ... character( * ), i.e., inferred at runtime
- $-2$ ... character(:), allocatable (possibly we might use -1 for that also)
- $-3$ ... character(n+3), i.e., a runtime expression stored in `len_expr`

**`kind`**: The `kind` member selects the kind of a given type. We currently
support the following:
- Integer kinds: 1 (i8), 2 (i16), 4 (i32), 8 (i64)
- Real kinds: 4 (f32), 8 (f64)
- Complex kinds: 4 (c32), 8 (c64)
- Character kinds: 1 (utf8 string)
- Logical kinds: 1, 2, 4: (boolean represented by 1, 2, 4 bytes; the default
kind is 4, just like the default integer kind, consistent with Python
and Fortran: in Python, "Booleans in Python are implemented as a subclass
of integers"; in Fortran the "default logical kind has the same storage
size as the default integer"; we currently use kind=4 as default
integer, so we also use kind=4 for the default logical.)

### String format kind
```asdl
string_format_kind
    = FormatFortran        -- "(f8.3,i4.2)", a, b
    | FormatC              -- "%f: %d", a, b
    | FormatPythonPercent  -- "%f: %d" % (a, b)
    | FormatPythonFString  -- f"{a}: {b}"
    | FormatPythonFormat   -- "{}: {}".format(a, b)
```
