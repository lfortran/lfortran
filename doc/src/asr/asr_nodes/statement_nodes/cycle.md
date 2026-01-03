# Cycle

Cycle function, a **statement (stmt)** node.

## Declaration

### Syntax

```fortran
Cycle()
```

### Arguments

None.

### Return values

None.

## Description

**cycle** skips whatever is left of the loop and goes into the next cycle.

## Types

Not applicable.

## Examples

```fortran
program doloop_03
    implicit none
    integer :: i, j

    j = 0
    do i = 1, 10
        if (i == 2) cycle
        j = j + i
    end do
    if (j /= 53) error stop
    print *, j
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            doloop_03:
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
                    doloop_03
                    []
                    [(=
                        (Var 2 j)
                        (IntegerConstant 0 (Integer 4 []))
                        ()
                    )
                    (DoLoop
                        ((Var 2 i)
                        (IntegerConstant 1 (Integer 4 []))
                        (IntegerConstant 10 (Integer 4 []))
                        ())
                        [(If
                            (IntegerCompare
                                (Var 2 i)
                                Eq
                                (IntegerConstant 2 (Integer 4 []))
                                (Logical 4 [])
                                ()
                            )
                            [(Cycle)]
                            []
                        )
                        (=
                            (Var 2 j)
                            (IntegerBinOp
                                (Var 2 j)
                                Add
                                (Var 2 i)
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
                            (IntegerConstant 53 (Integer 4 []))
                            (Logical 4 [])
                            ()
                        )
                        [(ErrorStop
                            ()
                        )]
                        []
                    )
                    (Print
                        ()
                        [(Var 2 j)]
                        ()
                        ()
                    )]
                )

        })
    []
)

```

## See Also
