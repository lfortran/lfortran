# Variable

Variable is a **symbol** node representing a variable declaration.

## Declaration

### Syntax

```
Variable(symbol_table parent_symtab, identifier name, identifier* dependencies,
    intent intent, expr? symbolic_value, expr? value, storage_type storage,
    ttype type, symbol type_declaration,
    abi abi, access access, presence presence, bool value_attr)
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


### Return values

None.

## Description

A `Variable` node represents a declaration of any variable in the
program. It contais information about the type, visibility, compile-time value,
etc.

The type of the variable can be any of the primitive types like integer,
real, complex, pointers, arrays. In such cases, the `type_declaration` member of
the `Variable` is null.

`Variable` might also have a non-primitive type like `StructType`, or types for
classes, enums, and function pointers. Such types are not declared inline to the
`Variable` node itself. In such cases, the `type_declaration` member of
`Variable` points to the symbol containing the declaration of the type.

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

[Var](../expression_nodes/Var.md)
