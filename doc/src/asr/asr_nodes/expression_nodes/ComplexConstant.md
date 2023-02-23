# ComplexConstant

Complex literal constant, an `expr` node.

## Declaration

### Syntax

```fortran
ComplexConstant(floar re, float im, ttype type)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
| `re`           | value of real  part |
| `im`          | value of imaginary part |
| `type`        | tabel entry type |

### Return values

The return value is the expression that the ComplexConstant represents.

## Description

**ComplexConstant** represents complex constant which consists of a optional
imaginary part. Both real and imaginary part can have integer or floating point
values, followed by a string of decimal digits, before or after
decimal point. If no sign is present, the constant is assumed to be non negative.

The value must be in the `Complex*8` range, uses 8 bytes of storage.
The constants are separated by a comma, and the pair is enclosed in parentheses.
The first constant is the real part, and the second is the imaginary part.

## Types

Only accepts complex contants.

## Examples


```fortran
(1.0, 2.1)
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(ComplexConstructor
        (RealConstant
            1.000000
            (Real 4 [])
        )
        (RealConstant
            2.100000
            (Real 4 [])
        )
        (Complex 4 [])
        (ComplexConstant
            1.000000
            2.100000
            (Complex 4 [])
        )
    )]
)

```

## See Also

[IntegerConstant](IntegerConstant.md), [RealConstant](RealConstant.md).
