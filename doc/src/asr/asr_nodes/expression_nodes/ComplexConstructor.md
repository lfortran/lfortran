# ComplexConstructor

Complex constructor expression.

## Declaration

### Syntax

```fortran
ComplexConstructor(expr re, expr im, ttype type, expr? value)
```

### Arguments

| Argument name | Argument Description |
| --------------|----------------------|
| `re`          | real expression      |
| `im`          | imaginary expression |
| `ttype`       | table entry type     |
| `value`       | expression value     |

### Return values

The return value is the expression that the ComplexConstructor represents.

## Description

**ComplexConstructor** represents constructor of complex type. A complex literal
constant is a complex constructor where each expression if a pair of initialisation
expressions.

## Types

Only accepts complex and real types. The imaginary part can be 0 or can have value.

## Examples

```fortran
program complex
    integer :: i = 42
    real :: x = 3.14
    complex :: z1, z2
    z1 = cmplx(i, x)
    print *, z1, cmplx(x, kind=8)
    z2 = cmplx(z2, kind=8)
end program complex
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            complex:
                (Program
                    (SymbolTable
                        2
                        {
                            i:
                                (Variable
                                    2
                                    i
                                    Local
                                    (IntegerConstant 42 (Integer 4 []))
                                    ()
                                    Save
                                    (Integer 4 [])
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
                                    (RealConstant
                                        3.140000
                                        (Real 4 [])
                                    )
                                    ()
                                    Save
                                    (Real 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            z1:
                                (Variable
                                    2
                                    z1
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
                            z2:
                                (Variable
                                    2
                                    z2
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
                    complex
                    []
                    [(=
                        (Var 2 z1)
                        (ComplexConstructor
                            (Var 2 i)
                            (Var 2 x)
                            (Complex 4 [])
                            ()
                        )
                        ()
                    )
                    (Print
                        ()
                        [(Var 2 z1)
                        (ComplexConstructor
                            (Var 2 x)
                            (RealConstant
                                0.000000
                                (Real 8 [])
                            )
                            (Complex 8 [])
                            ()
                        )]
                        ()
                        ()
                    )
                    (=
                        (Var 2 z2)
                        (Var 2 z2)
                        ()
                    )]
                )

        })
    []
)

```

## See Also

