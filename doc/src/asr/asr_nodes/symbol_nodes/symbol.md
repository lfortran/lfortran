# symbol

A textual denotation having special meaning to the compiler. The value can be
determined from the text of the basic *symbol*.

## Declaration

### Syntax

```fortran
symbol
    = Program(symbol_table symtab, identifier name, identifier* dependencies,
        stmt* body)
    | Module(symbol_table symtab, identifier name, identifier* dependencies,
        bool loaded_from_mod, bool intrinsic)
    | Function(symbol_table symtab, identifier name, expr* args, stmt* body,
        expr? return_var, abi abi, access access, deftype deftype,
        string? bindc_name, bool elemental, bool pure, bool module, bool inline,
        bool static, ttype* type_params, symbol* restrictions, bool is_restriction)
    | GenericProcedure(symbol_table parent_symtab, identifier name,
        symbol* procs, access access)
    | CustomOperator(symbol_table parent_symtab, identifier name,
        symbol* procs, access access)
    | ExternalSymbol(symbol_table parent_symtab, identifier name,
        symbol external, identifier module_name, identifier* scope_names,
        identifier original_name, access access)
    | StructType(symbol_table symtab, identifier name, identifier* members,
        abi abi, access access, symbol? parent)
    | EnumType(symbol_table symtab, identifier name, identifier* members,
        abi abi, access access, enumtype enum_value_type, ttype type, symbol? parent)
    | UnionType(symbol_table symtab, identifier name, identifier* members,
        abi abi, access access, symbol? parent)
    | Variable(symbol_table parent_symtab, identifier name, intent intent,
        expr? symbolic_value, expr? value, storage_type storage, ttype type,
        abi abi, access access, presence presence, bool value_attr)
    | ClassType(symbol_table symtab, identifier name, abi abi, access access)
    | ClassProcedure(symbol_table parent_symtab, identifier name,
    identifier?
        self_argument, identifier proc_name, symbol proc, abi abi)
    | AssociateBlock(symbol_table symtab, identifier name, stmt* body)
    | Block(symbol_table symtab, identifier name, stmt* body)
    | Requirement(symbol_table symtab, identifier name, identifier* args, require_instantiation* requires)
    | Template(symbol_table symtab, identifier name, identifier* args, require_instantiation* requires)
```

### Arguments

| Argument Name              | Denotes                    |
|----------------------------|----------------------------|
| `symtab`                   |local symbol table          |
| `name`                     |symbol name for easy lookup |
| `dependencies`             |dependencies of symbol      |
| `body`                     |statement body              |
| `loaded_from_mod`          |boolean value if loaded from mode |
| `intrinsic`                |boolean value if intrinsic        |
| `args`                     |arguments of expression     |
| `abi`                      |source                      |
| `original_name`            |original symbol             |
| `parent_symtab`            |where symbol is stored      |
| `scope_names`              |list of names if the symbol is in a nested symbol table |
| `storage_type`             | Default/Save/Parameter/Allocatable |
| `access`                   | Public/Private             |
| `intent`                   |Local/In/Out/InOut/ReturnVar/Unspecified|
| `deftype`                  |Implementation/Interface|
| `presence`                 |Required/Optional|
| `self_argument`            |the argument which contains the object calling the class procedure |
| `requires`                 |assigning types to requirement or template arguments    |

### Return values

None.

## Description

Each symbol has either `symtab` (local symbol table) or `parent_symtab` (where
this symbol is stored). One can get to parent_symtab via symtab, so only one is
present.

Each symbol has a `name` for easy lookup of the name of the symbol when only
having a pointer to it.

abi=Source means the symbol's implementation is included (full ASR), otherwise
it is external (interface ASR, such as procedure interface).

SubroutineCall/FunctionCall store the actual final resolved subroutine or function
(`name` member). They also store the original symbol (`original_name`), which
can be one of: null, GenericProcedure or ExternalSymbol.

When a module is compiled, it is parsed into full ASR, an object file is produced,
the full ASR (abi=Source, "body" is non-empty) is transformed into interface
ASR (abi=LFortran, "body" is empty). Both interface and full ASR is saved into
the mod file.

When a module is used, it is first looked up in the symbol table (as either full
or interface ASR) and used if it is present. Otherwise a mod file is found on
the disk, loaded (as either full or interface ASR for LFortran's mod file,
depending on LFortran's compiler options; or for GFortran's mod file the
corresponding interface ASR is constructed with abi=GFortran) and used. After the
ASR is loaded, the symbols that are used are represented as ExternalSymbols in
the current scope of the symbol table.

ExternalSymbol represents symbols that cannot be looked up in the current scoped
symbol table. As an example, if a variable is defined in a module, but used in a
nested subroutine, that is not an external symbol because it can be resolved in
the current symbol table (nested subroutine) by following the parents. However
if a symbol is used from a different module, then it is an external symbol,
because usual symbol resolution by going to the parents will not find the
definition. The `module_name` member is the name of the module the symbol is in,
the `scope_names` is a list of names if the symbol is in a nested symbol table.
For example if it is a local variable in a function `f` that is nested in function
`g`, then `scope_names=[g, f]`.

REPL: each cell is parsed into full ASR, compiled + executed, the full ASR is
transformed into interface ASR (abi=LFortran) and kept in the symbol table. A
new cell starts with an empty symbol table, whose parent symbol table is the
previous cell. That allows function / declaration shadowing.

Symbols in LFortran are:

1. Program
2. Module
3. Function
4. GenericProcedure
5. CustomOperator
6. ExternalSymbol
7. DerivedType
8. Variable
9. ClassType
10. ClassProcedure
11. AssociateBlock
12. Block
13. Requirement
14. Template

## Types

Special meaning textual denotations.

## Examples

Example of function:

```fortran
integer function a()
end
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            a:
                (Function
                    (SymbolTable
                        2
                        {
                            a:
                                (Variable
                                    2
                                    a
                                    ReturnVar
                                    ()
                                    ()
                                    Default
                                    (Integer 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                )

                        })
                    a
                    []
                    []
                    (Var 2 a)
                    Source
                    Public
                    Implementation
                    ()
                    .false.
                    .false.
                    .false.
                    .false.
                    .false.
                    []
                    []
                    .false.
                )

        })
    []
)
```

Example of GenericProcedure:

```fortran
module stdlib_quadrature_simps
    interface simps38_weights
        module procedure simps38_weights_dp
    end interface simps38_weights

contains

    function simps38_weights_dp(x) result(w)
        real(8), intent(in) :: x(4)
        real(8) :: w(size(x))
    end function simps38_weights_dp

    subroutine simps38_weights_dp_use(x1)
        real(8), intent(in) :: x1(4)
        print *, simps38_weights(x1)
    end subroutine simps38_weights_dp_use
end module

program stdlib_quadrature
use stdlib_quadrature_simps, only: simps38_weights_dp
implicit none
real(8) :: x1(4)
print *, simps38_weights_dp(x1)
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            stdlib_quadrature:
                (Program
                    (SymbolTable
                        5
                        {
                            simps38_weights_dp:
                                (ExternalSymbol
                                    5
                                    simps38_weights_dp
                                    2 simps38_weights_dp
                                    stdlib_quadrature_simps
                                    []
                                    simps38_weights_dp
                                    Public
                                ),
                            x1:
                                (Variable
                                    5
                                    x1
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Real 8 [((IntegerConstant 1 (Integer 4 []))
                                    (IntegerConstant 4 (Integer 4 [])))])
                                    Source
                                    Public
                                    Required
                                    .false.
                                )

                        })
                    stdlib_quadrature
                    [stdlib_quadrature_simps]
                    [(Print
                        ()
                        [(FunctionCall
                            5 simps38_weights_dp
                            ()
                            [((Var 5 x1))]
                            (Real 8 [((IntegerConstant 1 (Integer 4 []))
                            (IntegerConstant 4 (Integer 4 [])))])
                            ()
                            ()
                        )]
                        ()
                        ()
                    )]
                ),
            stdlib_quadrature_simps:
                (Module
                    (SymbolTable
                        2
                        {
                            simps38_weights:
                                (GenericProcedure
                                    2
                                    simps38_weights
                                    [2 simps38_weights_dp]
                                    Public
                                ),
                            simps38_weights_dp:
                                (Function
                                    (SymbolTable
                                        3
                                        {
                                            w:
                                                (Variable
                                                    3
                                                    w
                                                    ReturnVar
                                                    ()
                                                    ()
                                                    Default
                                                    (Real 8 [((IntegerConstant 1 (Integer 4 []))
                                                    (IntegerConstant 4 (Integer 4 [])))])
                                                    Source
                                                    Public
                                                    Required
                                                    .false.
                                                ),
                                            x:
                                                (Variable
                                                    3
                                                    x
                                                    In
                                                    ()
                                                    ()
                                                    Default
                                                    (Real 8 [((IntegerConstant 1 (Integer 4 []))
                                                    (IntegerConstant 4 (Integer 4 [])))])
                                                    Source
                                                    Public
                                                    Required
                                                    .false.
                                                )

                                        })
                                    simps38_weights_dp
                                    [(Var 3 x)]
                                    []
                                    (Var 3 w)
                                    Source
                                    Public
                                    Implementation
                                    ()
                                    .false.
                                    .false.
                                    .false.
                                    .false.
                                    .false.
                                    []
                                    []
                                    .false.
                                ),
                            simps38_weights_dp_use:
                                (Function
                                    (SymbolTable
                                        4
                                        {
                                            x1:
                                                (Variable
                                                    4
                                                    x1
                                                    In
                                                    ()
                                                    ()
                                                    Default
                                                    (Real 8 [((IntegerConstant 1 (Integer 4 []))
                                                    (IntegerConstant 4 (Integer 4 [])))])
                                                    Source
                                                    Public
                                                    Required
                                                    .false.
                                                )

                                        })
                                    simps38_weights_dp_use
                                    [(Var 4 x1)]
                                    [(Print
                                        ()
                                        [(FunctionCall
                                            2 simps38_weights_dp
                                            2 simps38_weights
                                            [((Var 4 x1))]
                                            (Real 8 [((IntegerConstant 1 (Integer 4 []))
                                            (IntegerConstant 4 (Integer 4 [])))])
                                            ()
                                            ()
                                        )]
                                        ()
                                        ()
                                    )]
                                    ()
                                    Source
                                    Public
                                    Implementation
                                    ()
                                    .false.
                                    .false.
                                    .false.
                                    .false.
                                    .false.
                                    []
                                    []
                                    .false.
                                )

                        })
                    stdlib_quadrature_simps
                    []
                    .false.
                    .false.
                )

        })
    []
)
```

Example of Module and CustomOperator:

```fortran
module stdlib_string_type
    type :: string_type
        sequence
        private
        character(len=:), allocatable :: raw
    end type string_type

    interface write(formatted)
        module procedure :: write_formatted
    end interface

    interface read(formatted)
        module procedure :: read_formatted
    end interface

contains

subroutine write_formatted(string, unit, iotype, v_list, iostat, iomsg)
    type(string_type), intent(in) :: string
    integer, intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
end subroutine write_formatted

subroutine read_formatted(string, unit, iotype, v_list, iostat, iomsg)
    type(string_type), intent(inout) :: string
    integer, intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
    character(len=:), allocatable :: line
end subroutine read_formatted

end module

program string_14
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            stdlib_string_type:
                (Module
                    (SymbolTable
                        2
                        {
                            formatted:
                                (CustomOperator
                                    2
                                    formatted
                                    [2 read_formatted]
                                    Public
                                ),
                            read_formatted:
                                (Function
                                    (SymbolTable
                                        5
                                        {
                                            iomsg:
                                                (Variable
                                                    5
                                                    iomsg
                                                    InOut
                                                    ()
                                                    ()
                                                    Default
                                                    (Character 1 -1 () [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                ),
                                            iostat:
                                                (Variable
                                                    5
                                                    iostat
                                                    Out
                                                    ()
                                                    ()
                                                    Default
                                                    (Integer 4 [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                ),
                                            iotype:
                                                (Variable
                                                    5
                                                    iotype
                                                    In
                                                    ()
                                                    ()
                                                    Default
                                                    (Character 1 -1 () [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                ),
                                            line:
                                                (Variable
                                                    5
                                                    line
                                                    Local
                                                    ()
                                                    ()
                                                    Allocatable
                                                    (Character 1 -2 () [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                ),
                                            string:
                                                (Variable
                                                    5
                                                    string
                                                    InOut
                                                    ()
                                                    ()
                                                    Default
                                                    (Struct
                                                        2 string_type
                                                        []
                                                    )
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                ),
                                            unit:
                                                (Variable
                                                    5
                                                    unit
                                                    In
                                                    ()
                                                    ()
                                                    Default
                                                    (Integer 4 [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                ),
                                            v_list:
                                                (Variable
                                                    5
                                                    v_list
                                                    In
                                                    ()
                                                    ()
                                                    Default
                                                    (Integer 4 [(()
                                                    ())])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                )

                                        })
                                    read_formatted
                                    [(Var 5 string)
                                    (Var 5 unit)
                                    (Var 5 iotype)
                                    (Var 5 v_list)
                                    (Var 5 iostat)
                                    (Var 5 iomsg)]
                                    [(ImplicitDeallocate
                                        [5 line]
                                    )]
                                    ()
                                    Source
                                    Private
                                    Implementation
                                    ()
                                    .false.
                                    .false.
                                    .false.
                                    .false.
                                    .false.
                                    []
                                    []
                                    .false.
                                ),
                            string_type:
                                (StructType
                                    (SymbolTable
                                        3
                                        {
                                            raw:
                                                (Variable
                                                    3
                                                    raw
                                                    Local
                                                    ()
                                                    ()
                                                    Allocatable
                                                    (Character 1 -2 () [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                )

                                        })
                                    string_type
                                    [raw]
                                    Source
                                    Private
                                    ()
                                ),
                            write_formatted:
                                (Function
                                    (SymbolTable
                                        4
                                        {
                                            iomsg:
                                                (Variable
                                                    4
                                                    iomsg
                                                    InOut
                                                    ()
                                                    ()
                                                    Default
                                                    (Character 1 -1 () [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                ),
                                            iostat:
                                                (Variable
                                                    4
                                                    iostat
                                                    Out
                                                    ()
                                                    ()
                                                    Default
                                                    (Integer 4 [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                ),
                                            iotype:
                                                (Variable
                                                    4
                                                    iotype
                                                    In
                                                    ()
                                                    ()
                                                    Default
                                                    (Character 1 -1 () [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                ),
                                            string:
                                                (Variable
                                                    4
                                                    string
                                                    In
                                                    ()
                                                    ()
                                                    Default
                                                    (Struct
                                                        2 string_type
                                                        []
                                                    )
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                ),
                                            unit:
                                                (Variable
                                                    4
                                                    unit
                                                    In
                                                    ()
                                                    ()
                                                    Default
                                                    (Integer 4 [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                ),
                                            v_list:
                                                (Variable
                                                    4
                                                    v_list
                                                    In
                                                    ()
                                                    ()
                                                    Default
                                                    (Integer 4 [(()
                                                    ())])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                )

                                        })
                                    write_formatted
                                    [(Var 4 string)
                                    (Var 4 unit)
                                    (Var 4 iotype)
                                    (Var 4 v_list)
                                    (Var 4 iostat)
                                    (Var 4 iomsg)]
                                    []
                                    ()
                                    Source
                                    Private
                                    Implementation
                                    ()
                                    .false.
                                    .false.
                                    .false.
                                    .false.
                                    .false.
                                    []
                                    []
                                    .false.
                                )

                        })
                    stdlib_string_type
                    []
                    .false.
                    .false.
                ),
            string_14:
                (Program
                    (SymbolTable
                        6
                        {

                        })
                    string_14
                    []
                    []
                )

        })
    []
)
```

Example of ClassProcedure:

```fortran
module bitset
    type, abstract :: bitset_type

        private
        integer(8) :: num_bits

    contains

        procedure(all_abstract), deferred, pass(self) :: all

    end type bitset_type


    abstract interface

        elemental function all_abstract( self ) result(all)
            import :: bitset_type
            logical :: all
            class(bitset_type), intent(in) :: self
        end function all_abstract
    end interface

end module

program debug
    implicit none
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            bitset:
                (Module
                    (SymbolTable
                        2
                        {
                            all_abstract:
                                (Function
                                    (SymbolTable
                                        4
                                        {
                                            all:
                                                (Variable
                                                    4
                                                    all
                                                    ReturnVar
                                                    ()
                                                    ()
                                                    Default
                                                    (Logical 4 [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                ),
                                            self:
                                                (Variable
                                                    4
                                                    self
                                                    In
                                                    ()
                                                    ()
                                                    Default
                                                    (Class
                                                        2 bitset_type
                                                        []
                                                    )
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                )

                                        })
                                    all_abstract
                                    [(Var 4 self)]
                                    []
                                    (Var 4 all)
                                    Source
                                    Private
                                    Interface
                                    ()
                                    .true.
                                    .false.
                                    .false.
                                    .false.
                                    .false.
                                    []
                                    []
                                    .false.
                                ),
                            bitset_type:
                                (StructType
                                    (SymbolTable
                                        3
                                        {
                                            all:
                                                (ClassProcedure
                                                    3
                                                    all
                                                    all_abstract
                                                    2 all_abstract
                                                    Source
                                                ),
                                            num_bits:
                                                (Variable
                                                    3
                                                    num_bits
                                                    Local
                                                    ()
                                                    ()
                                                    Default
                                                    (Integer 8 [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                )

                                        })
                                    bitset_type
                                    [num_bits]
                                    Source
                                    Private
                                    ()
                                )

                        })
                    bitset
                    []
                    .false.
                    .false.
                ),
            debug:
                (Program
                    (SymbolTable
                        5
                        {

                        })
                    debug
                    []
                    []
                )

        })
    []
)
```

## See Also
