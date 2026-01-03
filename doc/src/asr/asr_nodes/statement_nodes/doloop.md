# DoLoop

The `DO` statement to repeatedly execute a set of statements.

## Declaration

### Syntax

```fortran
DoLoop(do_loop_head head, stmt* body)
```

### Arguments

`head` contains do loop header or loop control.
`body` contains do loop statements.

### Return values

None.

## Description

**DoLoop** statement nodes repeatedly executes a set of statements.

A labeled `DO` loop consists of the following:
- `DO` statement
- Set of executable statements i.e., body of the `do` loop
- Terminal statement, like `CONTINUE` statement

The `DO` variable must not be modified in any way within the range of the `DO`
loop.

Control must not jump into the range of a `DO` loop from outside its range.

After the terminal statement of a `DO` loop is executed, the following steps are
performned:
- The value of the `DO` variable, if any, is incremented.
- The iteration count of the loop statement is decremented by one.
- The iteration count of condition is tested.

## Types

Accessible global identifier and a sequence of zero or more statements or
construst that make the `DO` range.

## Examples

```fortran
program doloop
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

[doconcurrentloop](doconcurrentloop.md)
