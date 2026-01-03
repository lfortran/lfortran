# IntegerBOZ

Integer typeless constants, Binary, Octal, and Hexadecimal.

## Declaration

### Syntax

```fortran
IntegerBOZ(int v, integerboz intboz_type, ttype? type)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
| `v`           | integer value |
| `intboz_type` | integerboz = Binary | Hex | Octal |
| `type`        | table entry type |

### Return values

The return value is the expression that the IntegerBOZ represents.

## Description

**IntegerBOZ** represents integer **B**inary **O**ctal **H/Z**exadecimal
constants.

These are typeless numeric constants as their expressions assume data types
based on how they are used.

These constants are enclosed a string of appropriate digits in apostrophes and
prefix it with the letter B, O, X, or Z.

## Types

Only accepts integers and real.

## Examples

```fortran
B'0001000'
O'777'
Z'FFF99A'
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(IntegerBOZ
        8
        Binary
        ()
    )
    (IntegerBOZ
        511
        Octal
        ()
    )
    (IntegerBOZ
        16775578
        Hex
        ()
    )]
)

```

## See Also

