# DoConcurrentLoop

DoConcurrentLoop specifies that there are no data dependencies between the
iterations of a DO loop.

## Declaration

### Syntax

```fortran
DoConcurrentLoop(do_loop_head head, stmt* body)
```

### Arguments

`head` contains do loop concurrent header.
`body` contains loop body.

### Return values

None.

## Description

**DoConcurrentLoop** specifies that there are no data dependecies between the
iterations of a DO loop. Within the body of a `DoConcurrent` loop the program
must adgere to a the list of restrictions specified on its use of Fortran
language features. Actions that can't be executed in parallel or that don't
allow all iterations to execute are prohibited, like:

- Control flow statements which prevents the loop nest from executing all its
iterations.
- Image control statements, like `STOP`, `SYNC`, `ALLOCATE/DEALLOCATE`.

## Types

Accessible global identifier and a sequence of zero or more statements or
construst that make the `DO` range.

## Examples

```fortran
program doconcurrentloop
implicit none

real, dimension(10) :: a
real :: sum
integer :: N, i

N = size(a)
sum = 0

do concurrent (i = 1:N)
    a(i) = a(i-1) + 5
end do

call arraySum(a, sum)

contains
    subroutine arraySum(a, sum)
        real, intent(in) :: a(:)
        real, intent(out) :: sum
        do concurrent (i = 1:N) reduce(+: s)
            sum = sum + a(i)
        end do
    end subroutine
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            doconcurrentloop_02:
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
                                    (Real 4 [((IntegerConstant 1 (Integer 4 []))
                                    (IntegerConstant 10 (Integer 4 [])))])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            arraysum:
                                (Function
                                    (SymbolTable
                                        3
                                        {
                                            a:
                                                (Variable
                                                    3
                                                    a
                                                    In
                                                    ()
                                                    ()
                                                    Default
                                                    (Real 4 [(()
                                                    ())])
                                                    Source
                                                    Public
                                                    Required
                                                    .false.
                                                ),
                                            sum:
                                                (Variable
                                                    3
                                                    sum
                                                    Out
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
                                    arraysum
                                    [(Var 3 a)
                                    (Var 3 sum)]
                                    [(DoConcurrentLoop
                                        ((Var 2 i)
                                        (IntegerConstant 1 (Integer 4 []))
                                        (Var 2 n)
                                        ())
                                        [(=
                                            (Var 3 sum)
                                            (RealBinOp
                                                (Var 3 sum)
                                                Add
                                                (ArrayItem
                                                    (Var 3 a)
                                                    [(()
                                                    (Var 2 i)
                                                    ())]
                                                    (Real 4 [])
                                                    ()
                                                )
                                                (Real 4 [])
                                                ()
                                            )
                                            ()
                                        )]
                                    )]
                                    ()
                                    Source
                                    Public
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
                            n:
                                (Variable
                                    2
                                    n
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
                            sum:
                                (Variable
                                    2
                                    sum
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
                    doconcurrentloop_02
                    []
                    [(=
                        (Var 2 n)
                        (ArraySize
                            (Var 2 a)
                            ()
                            (Integer 4 [])
                            (IntegerConstant 10 (Integer 4 []))
                        )
                        ()
                    )
                    (=
                        (Var 2 sum)
                        (Cast
                            (IntegerConstant 0 (Integer 4 []))
                            IntegerToReal
                            (Real 4 [])
                            (RealConstant
                                0.000000
                                (Real 4 [])
                            )
                        )
                        ()
                    )
                    (DoConcurrentLoop
                        ((Var 2 i)
                        (IntegerConstant 1 (Integer 4 []))
                        (Var 2 n)
                        ())
                        [(=
                            (ArrayItem
                                (Var 2 a)
                                [(()
                                (Var 2 i)
                                ())]
                                (Real 4 [])
                                ()
                            )
                            (RealBinOp
                                (ArrayItem
                                    (Var 2 a)
                                    [(()
                                    (IntegerBinOp
                                        (Var 2 i)
                                        Sub
                                        (IntegerConstant 1 (Integer 4 []))
                                        (Integer 4 [])
                                        ()
                                    )
                                    ())]
                                    (Real 4 [])
                                    ()
                                )
                                Add
                                (Cast
                                    (IntegerConstant 5 (Integer 4 []))
                                    IntegerToReal
                                    (Real 4 [])
                                    (RealConstant
                                        5.000000
                                        (Real 4 [])
                                    )
                                )
                                (Real 4 [])
                                ()
                            )
                            ()
                        )]
                    )
                    (SubroutineCall
                        2 arraysum
                        ()
                        [((Var 2 a))
                        ((Var 2 sum))]
                        ()
                    )]
                )

        })
    []
)
```

## See Also
