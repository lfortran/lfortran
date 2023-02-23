# Print

Print statement, a stmt node.

## Declaration

### Syntax

```fortran
Print(expr? fmt, expr* values, expr? separator, expr? end)
```

### Arguments

`fmt` contains format identifier
`values` contains pointer to values of expression.
`separator` contains separator for expressions.
`end` contains end of expressions.

### Return values

None.

## Description

**Print** statement writes from a list to `stdout`. `fmt` the format identifier
can be:

- An asterist (\*), which indicates list-directed I/O.
- The lable of a `FORMAT` statement that appears in the same program unit.
- An integer variable name that has been assigned the label of a `FORMAT`
statement that appears in the same program unit.
- A character expression or interger array that specifies the format string.

## Types

format indetifier and expressions.

## Examples

```fortran
program print
implicit none

    real :: pi
    pi = 3.141592

    print "(f6.3)", pi
    print *, pi
    print 10
10  format(3x, "print")
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            print1:
                (Program
                    (SymbolTable
                        2
                        {
                            pi:
                                (Variable
                                    2
                                    pi
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Real 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                )

                        })
                    print1
                    []
                    [(=
                        (Var 2 pi)
                        (RealConstant
                            3.141592
                            (Real 4 [])
                        )
                        ()
                    )
                    (Print
                        (StringConstant
                            "(f6.3)"
                            (Character 1 6 () [])
                        )
                        [(Var 2 pi)]
                        ()
                        ()
                    )
                    (Print
                        ()
                        [(Var 2 pi)]
                        ()
                        ()
                    )
                    (Print
                        (IntegerConstant 10 (Integer 4 []))
                        []
                        ()
                        ()
                    )
                    (GoToTarget
                        10
                        10
                    )]
                )

        })
    []
)
```

## See Also
