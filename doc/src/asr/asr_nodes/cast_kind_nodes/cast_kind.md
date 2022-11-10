# cast_kind

Cast Kind nodes or cast kind types.

## Declaration

### Syntax

```fortran
cast_kind
    = RealToInteger
    | IntegerToReal
    | LogicalToReal
    | RealToReal
    | IntegerToInteger
    | RealToComplex
    | IntegerToComplex
    | IntegerToLogical
    | RealToLogical
    | CharacterToLogical
    | CharacterToInteger
    | CharacterToList
    | ComplexToLogical
    | ComplexToComplex
    | ComplexToReal
    | ComplexToInteger
    | LogicalToInteger
    | RealToCharacter
    | IntegerToCharacter
    | LogicalToCharacter
```

### Arguments

None.

### Return values

None.

## Description

**cast_kind** nodes or cast kind types denotes kinds to typecast one idenfier to
another.

`cast_kind` denotes the types supported for cast in LFortran.

## Types

It denotes all types supported in LFortran, which are:

1. `RealToInteger` denotes `cast_kind` of `real` to `integer` type.
2. `IntegerToReal` denotes `cast_kind` of `integer` to `real` type.
3. `LogicalToReal` denotes `cast_kind` of `logical` to `real` type.
4. `RealToReal` denotes `cast_kind` of `Real` to `Real`.
5. `IntegerToInteger` denotes `cast_kind` of `integer` to `integer`.
6. `RealToComplex` denotes `cast_kind` of `Real` to `complex`.
7. `IntegerToComplex` denotes `cast_kind` of `integer` to `complex`.
8. `IntegerToLogical` denotes `cast_kind` of `integer` to `logical`.
9. `RealToLogical` denotes `cast_kind` of `real` to	`logical`.
10. `CharacterToLogical` denotes `cast_kind` of `character` to `logical`.
11. `CharacterToInteger` denotes `cast_kind` of `character` to `integer`.
12. `CharacterToList` denotes `cast_kind` of `character` to `list`.
13. `ComplexToLogical` denotes `cast_kind` of `complex` to `logical`.
14. `ComplexToComplex` denotes `cast_kind` of `complex` to `complex`.
15. `ComplexToReal` denotes `cast_kind` of `complex` to `real`.
16. `ComplexToInteger` denotes `cast_kind` of `complex` to `integer`.
17. `LogicalToInteger` denotes `cast_kind` of `logical` to `integer`.
18. `RealToCharacter` denotes `cast_kind` of `real` to `character`.
19. `IntegerToCharacter` denotes `cast_kind` of `integer` to `character`.
20. `LogicalToCharacter` denotes `cast_kind` of `logical` to `character`.

## Examples

Example for	`RealToInteger`:

```fortran
program types_03
implicit none
real :: r
integer :: i
r = 1.5
print *, r
i = r
print *, i
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            types_03:
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
                            r:
                                (Variable
                                    2
                                    r
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
                    types_03
                    []
                    [(=
                        (Var 2 r)
                        (RealConstant
                            1.500000
                            (Real 4 [])
                        )
                        ()
                    )
                    (Print
                        ()
                        [(Var 2 r)]
                        ()
                        ()
                    )
                    (=
                        (Var 2 i)
                        (Cast
                            (Var 2 r)
                            RealToInteger
                            (Integer 4 [])
                            ()
                        )
                        ()
                    )
                    (Print
                        ()
                        [(Var 2 i)]
                        ()
                        ()
                    )]
                )

        })
    []
)
```

Example for `IntegerToReal`:

```fortran
program types_02
implicit none
real :: r
integer :: i
i = 1
r = 1
r = i
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            types_02:
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
                            r:
                                (Variable
                                    2
                                    r
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
                    types_02
                    []
                    [(=
                        (Var 2 i)
                        (IntegerConstant 1 (Integer 4 []))
                        ()
                    )
                    (=
                        (Var 2 r)
                        (Cast
                            (IntegerConstant 1 (Integer 4 []))
                            IntegerToReal
                            (Real 4 [])
                            (RealConstant
                                1.000000
                                (Real 4 [])
                            )
                        )
                        ()
                    )
                    (=
                        (Var 2 r)
                        (Cast
                            (Var 2 i)
                            IntegerToReal
                            (Real 4 [])
                            ()
                        )
                        ()
                    )]
                )

        })
    []
)
```

Example of `RealToReal`:

```fortran
program types_01
implicit none
real :: r
r = 1.0
r = 1.5
r = 1.
r = float(2)
r = dble(3)
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            types_01:
                (Program
                    (SymbolTable
                        2
                        {
                            r:
                                (Variable
                                    2
                                    r
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
                    types_01
                    []
                    [(=
                        (Var 2 r)
                        (RealConstant
                            1.000000
                            (Real 4 [])
                        )
                        ()
                    )
                    (=
                        (Var 2 r)
                        (RealConstant
                            1.500000
                            (Real 4 [])
                        )
                        ()
                    )
                    (=
                        (Var 2 r)
                        (RealConstant
                            1.000000
                            (Real 4 [])
                        )
                        ()
                    )
                    (=
                        (Var 2 r)
                        (Cast
                            (Cast
                                (IntegerConstant 2 (Integer 4 []))
                                IntegerToReal
                                (Real 8 [])
                                (RealConstant
                                    2.000000
                                    (Real 8 [])
                                )
                            )
                            RealToReal
                            (Real 4 [])
                            (RealConstant
                                2.000000
                                (Real 4 [])
                            )
                        )
                        ()
                    )
                    (=
                        (Var 2 r)
                        (Cast
                            (Cast
                                (IntegerConstant 3 (Integer 4 []))
                                IntegerToReal
                                (Real 8 [])
                                (RealConstant
                                    3.000000
                                    (Real 8 [])
                                )
                            )
                            RealToReal
                            (Real 4 [])
                            (RealConstant
                                3.000000
                                (Real 4 [])
                            )
                        )
                        ()
                    )]
                )

        })
    []
)
```

Example of `IntegerToLogical`:

```fortran
program logical4
    ! this program checks logical operators
    implicit none

       ! variable declaration
       logical :: a, b, c

       ! assigning values
       a = 2
       b = -1
       c = 0

       print *, a, b, c

end program logical4
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            logical4:
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
                                    (Logical 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            b:
                                (Variable
                                    2
                                    b
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Logical 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            c:
                                (Variable
                                    2
                                    c
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Logical 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                )

                        })
                    logical4
                    []
                    [(=
                        (Var 2 a)
                        (Cast
                            (IntegerConstant 2 (Integer 4 []))
                            IntegerToLogical
                            (Logical 4 [])
                            ()
                        )
                        ()
                    )
                    (=
                        (Var 2 b)
                        (Cast
                            (IntegerUnaryMinus
                                (IntegerConstant 1 (Integer 4 []))
                                (Integer 4 [])
                                (IntegerConstant -1 (Integer 4 []))
                            )
                            IntegerToLogical
                            (Logical 4 [])
                            ()
                        )
                        ()
                    )
                    (=
                        (Var 2 c)
                        (Cast
                            (IntegerConstant 0 (Integer 4 []))
                            IntegerToLogical
                            (Logical 4 [])
                            ()
                        )
                        ()
                    )
                    (Print
                        ()
                        [(Var 2 a)
                        (Var 2 b)
                        (Var 2 c)]
                        ()
                        ()
                    )]
                )

        })
    []
)
```

Example of `ComplexToComplex`:

```fortran
program complex_dp

    complex(4) :: zero
    complex(8) :: v
    complex :: x
    zero = 0.0_4
    v = (1.05_4, 1.05_4)
    x = (1.05_4, 1.05_8)
    print *, v, x, zero

end program
```

ASR:

```
(TranslationUnit
    (SymbolTable
        1
        {
            complex_dp:
                (Program
                    (SymbolTable
                        2
                        {
                            v:
                                (Variable
                                    2
                                    v
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Complex 8 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            x:
                                (Variable
                                    2
                                    x
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Complex 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            zero:
                                (Variable
                                    2
                                    zero
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Complex 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                )

                        })
                    complex_dp
                    []
                    [(=
                        (Var 2 zero)
                        (Cast
                            (RealConstant
                                0.000000
                                (Real 4 [])
                            )
                            RealToComplex
                            (Complex 4 [])
                            (ComplexConstant
                                0.000000
                                0.000000
                                (Complex 4 [])
                            )
                        )
                        ()
                    )
                    (=
                        (Var 2 v)
                        (Cast
                            (ComplexConstructor
                                (RealConstant
                                    1.050000
                                    (Real 4 [])
                                )
                                (RealConstant
                                    1.050000
                                    (Real 4 [])
                                )
                                (Complex 4 [])
                                (ComplexConstant
                                    1.050000
                                    1.050000
                                    (Complex 4 [])
                                )
                            )
                            ComplexToComplex
                            (Complex 8 [])
                            ()
                        )
                        ()
                    )
                    (=
                        (Var 2 x)
                        (Cast
                            (ComplexConstructor
                                (RealConstant
                                    1.050000
                                    (Real 4 [])
                                )
                                (RealConstant
                                    1.050000
                                    (Real 8 [])
                                )
                                (Complex 8 [])
                                (ComplexConstant
                                    1.050000
                                    1.050000
                                    (Complex 8 [])
                                )
                            )
                            ComplexToComplex
                            (Complex 4 [])
                            ()
                        )
                        ()
                    )
                    (Print
                        ()
                        [(Var 2 v)
                        (Var 2 x)
                        (Var 2 zero)]
                        ()
                        ()
                    )]
                )

        })
    []
)
```

## See Also
