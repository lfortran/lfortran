# Assign

Assign function, a **statement (stmt)** node.

## Declaration

### Syntax

```fortran
Assign(int label, identifier variable)
```

### Arguments

`label` contains statement label.
`variable` contains integer variable.

### Return values

None.

## Description

**assign** statement assigns a statement label to a variable.

The label is the label of an executable statement or a FORMAT statement.
The integer variable, once assigned a statement label, can be reassigned the
same statement label, a different label, or an integer.

## Types


## Examples

```fortran
program assign
    integer :: x
    real :: y
10  format('(i3)')
    assign 10 to x
20  assign 10 to x
    ASSIGN 10 TO x
30  ASSIGN 10 TO y ! An integer variable is required in this context.
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            assign:
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
                                ),
                            y:
                                (Variable
                                    2
                                    y
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
                    assign
                    []
                    [(GoToTarget
                        10
                        10
                    )
                    (=
                        (Var 2 x)
                        (IntegerConstant 10 (Integer 4 []))
                        ()
                    )
                    (GoToTarget
                        20
                        20
                    )
                    (=
                        (Var 2 x)
                        (IntegerConstant 10 (Integer 4 []))
                        ()
                    )
                    (=
                        (Var 2 x)
                        (IntegerConstant 10 (Integer 4 []))
                        ()
                    )
                    (GoToTarget
                        30
                        30
                    )
                    (=
                        (Var 2 y)
                        (IntegerConstant 10 (Integer 4 []))
                        ()
                    )]
                )

        })
    []
)
```

## See Also
