# While Loop

While statement of `do while` loop, a `stmt` node.

## Declaration

### Syntax

```fortran
WhileLoop(expr test, stmt* body)
```

### Arguments

`test` contains expression to be tested.
`body` contains 0 or more statements or constructs.

### Return values

None.

## Description

**While** statement of `do while` loop constitues `test` expression to be evaulated
at each run on the `while` statement, and after first run.

Execution proceeds as:

1. The specified expression is evaluated.
2. If the value of the expression is true, the statements in the range of the
`DO WHILE` loop are executed.
3. If the value of the expression is false, control is transferred to the
statement following the `DO WHILE` loop.

## Types

Expression and pointer to body of the `DO WHILE loop`.

## Examples

```fortran
program while
implicit none
integer :: i, j
i = 1
j = 0
do while (i < 11)
    j = j + i
    i = i + 1
end do
if (j /= 55) error stop
if (i /= 11) error stop
end

```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            while_01:
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
                                ),
                            j:
                                (Variable
                                    2
                                    j
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
                    while_01
                    []
                    [(=
                        (Var 2 i)
                        (IntegerConstant 1 (Integer 4 []))
                        ()
                    )
                    (=
                        (Var 2 j)
                        (IntegerConstant 0 (Integer 4 []))
                        ()
                    )
                    (WhileLoop
                        (IntegerCompare
                            (Var 2 i)
                            Lt
                            (IntegerConstant 11 (Integer 4 []))
                            (Logical 4 [])
                            ()
                        )
                        [(=
                            (Var 2 j)
                            (IntegerBinOp
                                (Var 2 j)
                                Add
                                (Var 2 i)
                                (Integer 4 [])
                                ()
                            )
                            ()
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
                        )]
                    )
                    (If
                        (IntegerCompare
                            (Var 2 j)
                            NotEq
                            (IntegerConstant 55 (Integer 4 []))
                            (Logical 4 [])
                            ()
                        )
                        [(ErrorStop
                            ()
                        )]
                        []
                    )
                    (If
                        (IntegerCompare
                            (Var 2 i)
                            NotEq
                            (IntegerConstant 11 (Integer 4 []))
                            (Logical 4 [])
                            ()
                        )
                        [(ErrorStop
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
