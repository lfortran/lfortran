# IfArithmetic

The arithmetic `IF` statement branches to one of three specified statements,
depending on the value of an arithmetic expression.

## Declaration

### Syntax

```fortran
IfArithmetic(expr test, int lt_label, int eq_label, int gt_label)
```

### Arguments

`test` contains conditional expression to be evaluated.
`lt_label` contains
`eq_label` contains
`gt_label` contains 

### Return values

None.

## Description

**IfArithmetic** is the arithmetic `if` statement which branches to one of three
specified statements, depending on the value of an arithmetic expression.

## Types

`test` can be arithmetic expression of type integer, real, double precision, or
quadruple precision.

The other three lables are labels of executable statements.

## Examples

```fortran
program ifarithmetic
    integer :: i
    i = 5
    if (i == 5) print *, 'correct'
    if (i == 6) print *, 'incorrect'
    i = -2
40  i = i + 1
    if (i) 50, 60, 70
50  print *, 'i < 0'
    go to 40
60  print *, 'i == 0'
    go to 40
70  print *, 'i > 0'
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            if1:
                (Program
                    (SymbolTable
                        2
                        {
                            i:
                                (Variable
                                    2
                                    i
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
                    if1
                    []
                    [(=
                        (Var 2 i)
                        (IntegerConstant 5 (Integer 4 []))
                        ()
                    )
                    (If
                        (IntegerCompare
                            (Var 2 i)
                            Eq
                            (IntegerConstant 5 (Integer 4 []))
                            (Logical 4 [])
                            ()
                        )
                        [(Print
                            ()
                            [(StringConstant
                                "correct"
                                (Character 1 7 () [])
                            )]
                            ()
                            ()
                        )]
                        []
                    )
                    (If
                        (IntegerCompare
                            (Var 2 i)
                            Eq
                            (IntegerConstant 6 (Integer 4 []))
                            (Logical 4 [])
                            ()
                        )
                        [(Print
                            ()
                            [(StringConstant
                                "incorrect"
                                (Character 1 9 () [])
                            )]
                            ()
                            ()
                        )]
                        []
                    )
                    (=
                        (Var 2 i)
                        (IntegerUnaryMinus
                            (IntegerConstant 2 (Integer 4 []))
                            (Integer 4 [])
                            (IntegerConstant -2 (Integer 4 []))
                        )
                        ()
                    )
                    (GoToTarget
                        40
                        40
                    )
                    (=
                        (Var 2 i)
                        (IntegerBinOp
                            (Var 2 i)
                            Add
                            (IntegerConstant 1 (Integer 4 []))
                            (Integer 4 [])
                            ()
                        )
                        ()
                    )
                    (If
                        (IntegerCompare
                            (Var 2 i)
                            Lt
                            (IntegerConstant 0 (Integer 4 []))
                            (Logical 4 [])
                            ()
                        )
                        [(GoTo
                            50
                            50
                        )]
                        [(If
                            (IntegerCompare
                                (Var 2 i)
                                Gt
                                (IntegerConstant 0 (Integer 4 []))
                                (Logical 4 [])
                                ()
                            )
                            [(GoTo
                                70
                                70
                            )]
                            [(GoTo
                                60
                                60
                            )]
                        )]
                    )
                    (GoToTarget
                        50
                        50
                    )
                    (Print
                        ()
                        [(StringConstant
                            "i < 0"
                            (Character 1 5 () [])
                        )]
                        ()
                        ()
                    )
                    (GoTo
                        40
                        40
                    )
                    (GoToTarget
                        60
                        60
                    )
                    (Print
                        ()
                        [(StringConstant
                            "i == 0"
                            (Character 1 6 () [])
                        )]
                        ()
                        ()
                    )
                    (GoTo
                        40
                        40
                    )
                    (GoToTarget
                        70
                        70
                    )
                    (Print
                        ()
                        [(StringConstant
                            "i > 0"
                            (Character 1 5 () [])
                        )]
                        ()
                        ()
                    )]
                )

        })
    []
)

```

## See Also

[If](if.md)
