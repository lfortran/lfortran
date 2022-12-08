# Exit

Immediate termination of the program with status, a statement node.

## Declaration

### Syntax

```fortran
Exit()
```

### Arguments

None.

### Return values

`STATUS` is passed to the parent process or calling process on exit.

## Description

**Exit** causes immediate termination of the program with status. If status is
omitted it returns the canonical `succes` for the system.

It is useful for exit from loops or process.

## Types

Not applicable.

## Examples

```fortran
program exit
    implicit none
    integer :: i
    i = 1
    do
        i = i + 1
        if (i == 10) exit
    end do
end program exit
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            doloop_08:
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
                    doloop_08
                    []
                    [(=
                        (Var 2 i)
                        (IntegerConstant 1 (Integer 4 []))
                        ()
                    )
                    (WhileLoop
                        (LogicalConstant
                            .true.
                            (Logical 4 [])
                        )
                        [(=
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
                                Eq
                                (IntegerConstant 3 (Integer 4 []))
                                (Logical 4 [])
                                ()
                            )
                            [(Exit)]
                            []
                        )]
                    )]
                )

        })
    []
)
```

## See Also
