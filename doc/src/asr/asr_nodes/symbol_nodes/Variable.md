# Variable

Variable is a **symbol** node representing a variable declaration.

## Declaration

### Syntax

```
Variable(symbol_table parent_symtab, identifier name, identifier* dependencies,
    intent intent, expr? symbolic_value, expr? value, storage_type storage,
    ttype type, symbol? type_declaration,
    abi abi, access access, presence presence, bool value_attr,
    bool target_attr, bool contiguous_attr, string? bindc_name,
    bool is_volatile, bool is_protected,
    pass_attr pass_attr, identifier? self_argument)
```

### Arguments

`parent_symtab` integer id of the parent symbol table that contains the variable

`name` the name of the variable

`dependencies` other symbols that this variable depends on; must all be defined
in the `parent_symtab`

`intent` specifies intent (Local, `intent(in)`, `intent(inout)`, etc.)

`symbolic_value` the optional symbolic expression to initialize the variable
(e.g. `2+3+4+x`), this value must be compile time, but it is not necessarily a
constant (e.g., can contain binary operations, other variables, etc.)

`value` the optional constant expression holding the compile time value
(e.g. `5`, or `5.5`), it is a compile time constant.

`storage` whether `Save`, `Parameter`, `Allocatable`

`type` the ttype of the variable

`type_declaration` null for primitive types; for composite types that are
declared elsewhere in the program (struct, function, enum) it points to the
symbol that declares the type

`abi` abi such as: `Source`, `Interface`, `BindC`

`access` visibility: `Public`, `Private`

`presence` for parameters: `Required` or `Optional`

`value_attr` if true, this parameter has a `value` attribute set

`target_attr` if true, this variable has the `target` attribute

`contiguous_attr` if true, this variable has the `contiguous` attribute

`bindc_name` optional C binding name from `bind(c, name="...")`

`is_volatile` if true, this variable has the `volatile` attribute

`is_protected` if true, this variable has the `protected` attribute

`pass_attr` determines whether this variable is a procedure pointer component
with pass/nopass semantics. A three-valued enum:
- `NotMethod` — this is not a type-bound procedure pointer (default for all
  regular variables, standalone procedure pointers, and non-procedure variables)
- `Pass` — this is a procedure pointer component inside a derived type with
  pass semantics: the object through which it is called is implicitly passed
  as an argument at the position identified by `self_argument`
- `NoPass` — this is a procedure pointer component inside a derived type with
  nopass semantics: no object is passed implicitly

`self_argument` only meaningful when `pass_attr` is `Pass`. Identifies which
dummy argument of the procedure interface receives the passed object:
- `nullptr` — the passed object goes to the first dummy argument (default)
- a name (e.g. `"pt"`) — the passed object goes to the dummy argument with
  that name, which may be at any position in the argument list


### Return values

None.

## Description

A `Variable` node represents a declaration of any variable in the
program. It contains information about the type, visibility, compile-time value,
etc.

The type of the variable can be any of the primitive types like integer,
real, complex, pointers, arrays. In such cases, the `type_declaration` member of
the `Variable` is null.

`Variable` might also have a non-primitive type like `StructType`, or types for
classes, enums, and function pointers. Such types are not declared inline to the
`Variable` node itself. In such cases, the `type_declaration` member of
`Variable` points to the symbol containing the declaration of the type.

### Procedure Pointer Components

When a derived type declares a procedure pointer component, that component is
represented as a `Variable` whose `type` is a `FunctionType` (possibly wrapped
in a `Pointer`). The `pass_attr` and `self_argument` fields distinguish three
cases:

```fortran
type :: mytype
    ! NotMethod, self_argument=null: plain procedure pointer, no pass/nopass attribute
    procedure(iface), pointer :: op => null()
    ! NoPass, self_argument=null: explicitly no implicit self
    procedure(iface), nopass, pointer :: action => null()
    ! Pass, self_argument=null: self is passed as the first argument (default position)
    procedure(iface), pass, pointer :: scale => null()
    ! Pass, self_argument="self": self is passed at the position of dummy arg "self"
    procedure(iface), pass(self), pointer :: combine => null()
end type
```

For type-bound procedures declared with `contains` (not procedure pointer
components), see [StructMethodDeclaration](StructMethodDeclaration.md).

`Variable` represents declarations of variables. `Var` nodes represent instances
of variables in code. To represent the use of a variable in an expression,
employ the ASR `expr Var` node.

## Examples

```fortran
program expr2
integer :: x
x = (2+3)*5
print *, x
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            expr2:
                (Program
                    (SymbolTable
                        2
                        {
                            x:
                                (Variable
                                    2
                                    x
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Integer 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                    ()
                                )

                        })
                    expr2
                    []
                    [(=
                        (Var 2 x)
                        (IntegerBinOp
                            (IntegerBinOp
                                (IntegerConstant 2 (Integer 4 []))
                                Add
                                (IntegerConstant 3 (Integer 4 []))
                                (Integer 4 [])
                                (IntegerConstant 5 (Integer 4 []))
                            )
                            Mul
                            (IntegerConstant 5 (Integer 4 []))
                            (Integer 4 [])
                            (IntegerConstant 25 (Integer 4 []))
                        )
                        ()
                    )
                    (Print
                        ()
                        [(Var 2 x)]
                        ()
                        ()
                    )]
                )

        })
    []
)

```
## See Also

[Var](../expression_nodes/Var.md),
[StructMethodDeclaration](StructMethodDeclaration.md)
