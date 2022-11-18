# Assignment

Assignment statement, a **statement (stmt)** node.

## Declaration

### Syntax

```fortran
Assignment(expr target, expr value, stmt? overloaded)
```

### Arguments

`target` contains expression target.
`value` expressions giving the value to be assigned.
`overloaded` denotes if overloaded.

### Return values

None.

## Description

**assignment** statement assigns a value to a variable, substring, array element,
record, or record field.

The value can be a constant or the result of an expression. The kinds of assignment
statements: are arithmetic, logical, character, and record assignments.

## Types

Numeric type, a name of a variable, array element, or record field.
Arithmentic expression, chracter constant, or a logical expression.

## Examples

```fortran
module overload_assignment_m
    implicit none
    private
    public assignment (=)

    interface assignment (=)
        module procedure logical_gets_integer
    end interface
contains
    subroutine logical_gets_integer(tf, i)
        logical, intent (out) :: tf
        integer, intent (in)  :: i

        tf = (i == 0)
    end subroutine

    subroutine logical_gets_integer_use(tf, i)
        logical, intent (out) :: tf
        integer, intent (in)  :: i

        tf = i
    end subroutine
end module

program main
    use overload_assignment_m, only: assignment(=)
    implicit none
    logical :: tf

    tf = 0
    print *, "tf=0:", tf  ! Yields: T
    tf = 1
    print *, "tf=1:", tf  ! Yields: F
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
                        5
                        {
                            logical_gets_integer@~assign:
                                (ExternalSymbol
                                    5
                                    logical_gets_integer@~assign
                                    2 logical_gets_integer
                                    overload_assignment_m
                                    []
                                    logical_gets_integer
                                    Private
                                ),
                            tf:
                                (Variable
                                    5
                                    tf
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Logical 4 [])
                                    Source
                                    Private
                                    Required
                                    .false.
                                ),
                            ~assign:
                                (ExternalSymbol
                                    5
                                    ~assign
                                    2 ~assign
                                    overload_assignment_m
                                    []
                                    ~assign
                                    Private
                                )

                        })
                    main
                    [overload_assignment_m]
                    [(=
                        (Var 5 tf)
                        (IntegerConstant 0 (Integer 4 []))
                        (SubroutineCall
                            5 logical_gets_integer@~assign
                            5 ~assign
                            [((Var 5 tf))
                            ((IntegerConstant 0 (Integer 4 [])))]
                            ()
                        )
                    )
                    (Print
                        ()
                        [(StringConstant
                            "tf=0:"
                            (Character 1 5 () [])
                        )
                        (Var 5 tf)]
                        ()
                        ()
                    )
                    (=
                        (Var 5 tf)
                        (IntegerConstant 1 (Integer 4 []))
                        (SubroutineCall
                            5 logical_gets_integer@~assign
                            5 ~assign
                            [((Var 5 tf))
                            ((IntegerConstant 1 (Integer 4 [])))]
                            ()
                        )
                    )
                    (Print
                        ()
                        [(StringConstant
                            "tf=1:"
                            (Character 1 5 () [])
                        )
                        (Var 5 tf)]
                        ()
                        ()
                    )]
                ),
            overload_assignment_m:
                (Module
                    (SymbolTable
                        2
                        {
                            logical_gets_integer:
                                (Function
                                    (SymbolTable
                                        3
                                        {
                                            i:
                                                (Variable
                                                    3
                                                    i
                                                    In
                                                    ()
                                                    ()
                                                    Default
                                                    (Integer 4 [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                ),
                                            tf:
                                                (Variable
                                                    3
                                                    tf
                                                    Out
                                                    ()
                                                    ()
                                                    Default
                                                    (Logical 4 [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                )

                                        })
                                    logical_gets_integer
                                    [(Var 3 tf)
                                    (Var 3 i)]
                                    [(=
                                        (Var 3 tf)
                                        (IntegerCompare
                                            (Var 3 i)
                                            Eq
                                            (IntegerConstant 0 (Integer 4 []))
                                            (Logical 4 [])
                                            ()
                                        )
                                        ()
                                    )]
                                    ()
                                    Source
                                    Private
                                    Implementation
                                    ()
                                    .false.
                                    .false.
                                    .false.
                                    .false.
                                    .false.
                                    []
                                    []
                                    .false.
                                ),
                            logical_gets_integer_use:
                                (Function
                                    (SymbolTable
                                        4
                                        {
                                            i:
                                                (Variable
                                                    4
                                                    i
                                                    In
                                                    ()
                                                    ()
                                                    Default
                                                    (Integer 4 [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                ),
                                            tf:
                                                (Variable
                                                    4
                                                    tf
                                                    Out
                                                    ()
                                                    ()
                                                    Default
                                                    (Logical 4 [])
                                                    Source
                                                    Private
                                                    Required
                                                    .false.
                                                )

                                        })
                                    logical_gets_integer_use
                                    [(Var 4 tf)
                                    (Var 4 i)]
                                    [(=
                                        (Var 4 tf)
                                        (Var 4 i)
                                        (SubroutineCall
                                            2 logical_gets_integer
                                            2 ~assign
                                            [((Var 4 tf))
                                            ((Var 4 i))]
                                            ()
                                        )
                                    )]
                                    ()
                                    Source
                                    Private
                                    Implementation
                                    ()
                                    .false.
                                    .false.
                                    .false.
                                    .false.
                                    .false.
                                    []
                                    []
                                    .false.
                                ),
                            ~assign:
                                (CustomOperator
                                    2
                                    ~assign
                                    [2 logical_gets_integer]
                                    Public
                                )

                        })
                    overload_assignment_m
                    []
                    .false.
                    .false.
                )

        })
    []
)
```

## See Also
