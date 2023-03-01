# Variable

Variable symbol, a **symbol** node.

## Declaration

### Syntax

```
Variable(symbol_table parent_symtab, identifier name, identifier* dependencies,
    intent intent, expr? symbolic_value, expr? value, storage_type storage,
    ttype type, abi abi, access access, presence presence, bool value_attr)
```

### Arguments

`parent_symtab` the parent symbol table that contains the variable

`name` the name of the variable

`dependencies` other symbols that this variable depends on

`intent` specifies intent (Local, `intent(in)`, `intent(inout)`, etc.)

`symbolic_value` the optional symbolic expression to initialize the variable
(e.g. `2+3+4+x`), this value must be compile time, but it is not necessarily a
constant (e.g., can contain binary operations, other variables, etc.)

`value` the optional constant expression holding the compile time value
(e.g. `5`, or `5.5`), it is a compile time constant.

`storage` whether `Save`, `Parameter`, `Allocatable`

`type` the type of the variable

`abi` abi such as: `Source`, `Interface`, `BindC`

`access` visibility: `Public`, `Private`

`presence` for parameters: `Required` or `Optional`

`value_attr` if true, this parameter has a `value` attribute set

### Return values

None.

## Description

The `Variable` node is used to represent any variable in the program. It
contais information about the type, visibility, compile time value, etc.


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

[Module](Module.md), [Function](Function.md).
