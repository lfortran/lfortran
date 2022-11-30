# IntegerConstant

Integer literal constant, an `expr` node.

## Declaration

### Syntax

```fortran
IntegerConstant(int n, ttype type)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
| `n`           | value of integer |
| `type`        | tabel entry type |

### Return values

The return value is the expression that the IntegerConstant represents.

## Description

**IntegerConstant** represents integer constant which consists of a optional
plus or minus sign, followed by a string of decimal digits. No other characters
are allowed except, a space. If no sign is present, the constant is assumed to
be non negative.

The value must be in the `INTEGER*4` range (-2147483648, 2147483647).

If `INTEGER*8` range is (-9223372036854775808,9223372036854775807).

## Types

Only accepts integers.

## Examples


```fortran
+199
29002
-2147483648
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(IntegerConstant 199 (Integer 4 []))
    (IntegerConstant 29002 (Integer 4 []))
    (IntegerUnaryMinus
        (IntegerConstant 2147483648 (Integer 4 []))
        (Integer 4 [])
        (IntegerConstant -2147483648 (Integer 4 []))
    )]
)
```

## See Also

[RealConstant](RealConstant.md), [ComplexConstant](ComplexConstant.md).
