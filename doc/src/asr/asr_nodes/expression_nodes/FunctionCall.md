# FunctionCall

Function Call expression type.

## Declaration

### Syntax

```fortran
FunctionCall(symbol name, symbol? original_name, call_arg* args,
            ttype type, expr? value, expr? dt)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
| `name`        | symbol name |
| `original_name`| original name of the symbol |
| `args` | arguments of the called function |
| `type` | table entry type |
| `value` | expression value |
| `dt` | ? |

### Return values

The return value is the expression that the `FunctionCall` represents.

## Description

**FunctionCall** represents function call expression.

## Types

Not applicable.

## Examples

```fortran
program intrinsics
    integer, dimension(-1:1, -1:2) :: a
    print *, shape(a)             ! (/ 3, 4 /)
    print *, size(shape(42))      ! (/ /)
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            intrinsics:
                (Program
                    (SymbolTable
                        2
                        {
                            a:
                                (Variable
                                    2
                                    a
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Integer 4 [((IntegerUnaryMinus
                                        (IntegerConstant 1 (Integer 4 []))
                                        (Integer 4 [])
                                        (IntegerConstant -1 (Integer 4 []))
                                    )
                                    (IntegerConstant 3 (Integer 4 [])))
                                    ((IntegerUnaryMinus
                                        (IntegerConstant 1 (Integer 4 []))
                                        (Integer 4 [])
                                        (IntegerConstant -1 (Integer 4 []))
                                    )
                                    (IntegerConstant 4 (Integer 4 [])))])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            shape:
                                (ExternalSymbol
                                    2
                                    shape
                                    4 shape
                                    lfortran_intrinsic_builtin
                                    []
                                    shape
                                    Private
                                )

                        })
                    intrinsics
                    []
                    [(Print
                        ()
                        [(FunctionCall
                            2 shape
                            ()
                            [((Var 2 a))]
                            (Integer 4 [])
                            ()
                            ()
                        )]
                        ()
                        ()
                    )
                    (Print
                        ()
                        [(ArraySize
                            (FunctionCall
                                2 shape
                                ()
                                [((IntegerConstant 42 (Integer 4 [])))]
                                (Integer 4 [])
                                ()
                                ()
                            )
                            ()
                            (Integer 4 [])
                            (IntegerConstant 1 (Integer 4 []))
                        )]
                        ()
                        ()
                    )]
                ),
            lfortran_intrinsic_builtin:
                (IntrinsicModule lfortran_intrinsic_builtin)

        })
    []
)

```

## See Also

