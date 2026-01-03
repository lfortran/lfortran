# GoToTarget

A target of zero or more GoTo statements, a stmt node.

## Declaration

### Syntax

```fortran
GoToTarget(int id, identifier name)
```

### Arguments

`id` contains target unique id within a procedure.
`name` contins identifier name.

### Return values

None.

## Description

**GoToTarget** is an empty statement, a target of zero or more GoTo statements.
The `id` is only unique within a procedure.

## Types

Integer for `id` and name of the identifier.

## Examples

```fortran
program gototarget
1000 if (0<1) assign 15 to k
100 goto k
15 print *, "run"
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            main:
                (Program
                    (SymbolTable
                        2
                        {
                            k:
                                (Variable
                                    2
                                    k
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Integer 4 [])
                                    Source
                                    Public
                                    Optional
                                    .false.
                                )

                        })
                    main
                    []
                    [(GoToTarget
                        1000
                        1000
                    )
                    (If
                        (IntegerCompare
                            (IntegerConstant 0 (Integer 4 []))
                            Lt
                            (IntegerConstant 1 (Integer 4 []))
                            (Logical 4 [])
                            (LogicalConstant
                                .true.
                                (Logical 4 [])
                            )
                        )
                        [(=
                            (Var 2 k)
                            (IntegerConstant 15 (Integer 4 []))
                            ()
                        )]
                        []
                    )
                    (GoToTarget
                        100
                        100
                    )
                    (Select
                        (Var 2 k)
                        [(CaseStmt
                            [(IntegerConstant 100 (Integer 4 []))]
                            [(GoTo
                                100
                                100
                            )]
                        )
                        (CaseStmt
                            [(IntegerConstant 1000 (Integer 4 []))]
                            [(GoTo
                                1000
                                1000
                            )]
                        )
                        (CaseStmt
                            [(IntegerConstant 15 (Integer 4 []))]
                            [(GoTo
                                15
                                15
                            )]
                        )]
                        []
                    )
                    (GoToTarget
                        15
                        15
                    )
                    (Print
                        ()
                        [(StringConstant
                            "run"
                            (Character 1 3 () [])
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
