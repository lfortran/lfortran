# BlockCall

Call to block symbol, a `stmt` node.

## Declaration

### Syntax

```fortran
BlockCall(int label, symbol m)
```

### Arguments

`lable` contains integer value of the label.
`m` contains symbol passed to block call.

### Return values

None.

## Description

**BlockCall** signifies the block symbol call. Find more information on
[block](../symbol_nodes/symbol.md).

## Types

`label` must be of type integer.
`m` must be of type symbol.

## Examples

```fortran
program block
    integer :: a
    a = 10
    1 loop: block
        integer :: b
        a = a + 5
        if (a == 15) go to 1
        b = a / 2
        call square(b)
    end block loop
end program block

subroutine square(b)
    integer :: b, result
    result = b * b
    if (result /= 100) error stop
    print *, result
end subroutine square
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            block:
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
                                    (Integer 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            block:
                                (Block
                                    (SymbolTable
                                        4
                                        {
                                            b:
                                                (Variable
                                                    4
                                                    b
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
                                    block
                                    [(=
                                        (Var 2 a)
                                        (IntegerBinOp
                                            (Var 2 a)
                                            Add
                                            (IntegerConstant 5 (Integer 4 []))
                                            (Integer 4 [])
                                            ()
                                        )
                                        ()
                                    )
                                    (If
                                        (IntegerCompare
                                            (Var 2 a)
                                            Eq
                                            (IntegerConstant 15 (Integer 4 []))
                                            (Logical 4 [])
                                            ()
                                        )
                                        [(GoTo
                                            1
                                            1
                                        )]
                                        []
                                    )
                                    (=
                                        (Var 4 b)
                                        (IntegerBinOp
                                            (Var 2 a)
                                            Div
                                            (IntegerConstant 2 (Integer 4 []))
                                            (Integer 4 [])
                                            ()
                                        )
                                        ()
                                    )
                                    (SubroutineCall
                                        1 square
                                        ()
                                        [((Var 4 b))]
                                        ()
                                    )]
                                )

                        })
                    block
                    []
                    [(=
                        (Var 2 a)
                        (IntegerConstant 10 (Integer 4 []))
                        ()
                    )
                    (GoToTarget
                        1
                        1
                    )
                    (BlockCall
                        -1
                        2 block
                    )]
                ),
            square:
                (Function
                    (SymbolTable
                        3
                        {
                            b:
                                (Variable
                                    3
                                    b
                                    Unspecified
                                    ()
                                    ()
                                    Default
                                    (Integer 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            result:
                                (Variable
                                    3
                                    result
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
                    square
                    [(Var 3 b)]
                    [(=
                        (Var 3 result)
                        (IntegerBinOp
                            (Var 3 b)
                            Mul
                            (Var 3 b)
                            (Integer 4 [])
                            ()
                        )
                        ()
                    )
                    (If
                        (IntegerCompare
                            (Var 3 result)
                            NotEq
                            (IntegerConstant 100 (Integer 4 []))
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
                        [(Var 3 result)]
                        ()
                        ()
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
                )

        })
    []
)

```

## See Also
