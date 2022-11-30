# RealConstant

Real literal constant, an `expr` node.

## Declaration

### Syntax

```fortran
RealConstant(floar r, ttype type)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
| `r`           | value of real |
| `type`        | tabel entry type |

### Return values

The return value is the expression that the RealConstant represents.

## Description

**RealConstant** represents real constant which consists of a optional
plus or minus sign, followed by a string of decimal digits, before or after
decimal point. If no sign is present, the constant is assumed to be non negative.

The value must be in the `REAL*4` range, uses 4 bytes of storage. It can have a
decimal point or an exponent.

## Types

Only accepts real constant, real exponent, floating point values.

## Examples


```fortran
+199.
-1.2
1.6E12
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(RealConstant
        199.000000
        (Real 4 [])
    )
    (RealUnaryMinus
        (RealConstant
            1.200000
            (Real 4 [])
        )
        (Real 4 [])
        (RealConstant
            -1.200000
            (Real 4 [])
        )
    )
    (RealConstant
        1600000000000.000000
        (Real 4 [])
    )]
)

```

## See Also

[IntegerConstant](IntegerConstant.md), [ComplexConstant](ComplexConstant.md).
