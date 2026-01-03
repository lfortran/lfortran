# IntegerBitNot

Not conversion of integer bits, a `expr` node.

## Declaration

### Syntax

```fortran
IntegerBitNot(expr arg, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`arg`| expression arguments |
|`type`| table entry type |
|`value`|expression value |

### Return values

The return value is the expression that the IntegerBitNot represents.

## Description

**IntegerBitNot** represents integer binary not operation. It is used when flipping
bits of integer from `1` to `0` or `0` to `1`.

## Types

Only accepts integers.

## Examples

```fortran
not(1)
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            iso_fortran_env:
                (IntrinsicModule lfortran_intrinsic_iso_fortran_env),
            lfortran_intrinsic_bit:
                (IntrinsicModule lfortran_intrinsic_bit),
            not:
                (ExternalSymbol
                    1
                    not
                    3 not
                    lfortran_intrinsic_bit
                    []
                    not
                    Private
                )

        })
    [(IntegerBitNot
        (IntegerConstant 1 (Integer 4 []))
        (Integer 4 [])
        (IntegerConstant -2 (Integer 4 []))
    )]
)

```

## See Also

[LogicalNot](logicalnot.md)
