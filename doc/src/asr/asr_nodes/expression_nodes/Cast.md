# Cast

Cast to different type and/or kind.

## Declaration

### Syntax

```fortran
Cast(expr arg, cast_kind kind, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|   `arg`| expression argument  |
|`kind` | cast to kind |
|`type` | table entry type |
|`value`| expression value |

### Return values

The return value is the expression that the Cast represents.

## Description

**Cast** represents cast to different type and/or kind.

## Types

Only accepts integer, real.

## Examples

```fortran
real :: r
integer :: i
r = 1. * 2
i = 1. * 2
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            i:
                (Variable
                    1
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
                    1
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
    [(=
        (Var 1 r)
        (RealBinOp
            (RealConstant
                1.000000
                (Real 4 [])
            )
            Mul
            (Cast
                (IntegerConstant 2 (Integer 4 []))
                IntegerToReal
                (Real 4 [])
                (RealConstant
                    2.000000
                    (Real 4 [])
                )
            )
            (Real 4 [])
            (RealConstant
                2.000000
                (Real 4 [])
            )
        )
        ()
    )
    (=
        (Var 1 i)
        (Cast
            (RealBinOp
                (RealConstant
                    1.000000
                    (Real 4 [])
                )
                Mul
                (Cast
                    (IntegerConstant 2 (Integer 4 []))
                    IntegerToReal
                    (Real 4 [])
                    (RealConstant
                        2.000000
                        (Real 4 [])
                    )
                )
                (Real 4 [])
                (RealConstant
                    2.000000
                    (Real 4 [])
                )
            )
            RealToInteger
            (Integer 4 [])
            (IntegerConstant 2 (Integer 4 []))
        )
        ()
    )]
)

```

## See Also

