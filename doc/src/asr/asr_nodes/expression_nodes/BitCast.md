# BitCast

Cast bits to 1 or 0.

## Declaration

### Syntax

```fortran
BitCast(expr source, expr mold, expr? size, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|   `source`| source expression  |
|`mold` | mold expression |
|`size` | size of expression |
|`type` | table entry type |
|`value`| expression value |

### Return values

The return value is the expression that the BitCast represents.

## Description

**BitCast** represents casting bits to 0 or 1.

## Types

Only accepts integers.

## Examples

```fortran
integer :: x = 21432
print *, transfer(x, 1.0)
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            x:
                (Variable
                    1
                    x
                    Local
                    (IntegerConstant 21432 (Integer 4 []))
                    ()
                    Save
                    (Integer 4 [])
                    Source
                    Public
                    Required
                    .false.
                )

        })
    [(Print
        ()
        [(BitCast
            (Var 1 x)
            (RealConstant
                1.000000
                (Real 4 [])
            )
            ()
            (Real 4 [])
            ()
        )]
        ()
        ()
    )]
)

```

## See Also

