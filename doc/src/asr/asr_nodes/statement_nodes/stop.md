# Stop

To terminate execution of the program.

## Declaration

### Syntax

```fortran
Stop(expr? code)
```

### Arguments

`code` contains string of less than or equal to 5 digits or a character constant.

### Return values

None.

## Description

**Stop** statement terminates execution of the program. The argument `code` is
displayed when the program stops. If nothing is specified, no message is displayed.

## Types

Expression for optional `code` input parameter.

## Examples

```fortran
program stop
implicit none
integer :: x
x = (2+3)*5
if (x == 25) stop
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            stop:
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
                    stop
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
                    (If
                        (IntegerCompare
                            (Var 2 x)
                            Eq
                            (IntegerConstant 25 (Integer 4 []))
                            (Logical 4 [])
                            ()
                        )
                        [(Stop
                            ()
                        )]
                        []
                    )]
                )

        })
    []
)
```

## See Also

[Return](return.md)
