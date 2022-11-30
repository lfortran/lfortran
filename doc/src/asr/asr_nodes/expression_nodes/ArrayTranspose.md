# ArrayTranspose

Array or matrix transpose.

## Declaration

### Syntax

```fortran
ArrayTranspose(expr matrix, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`matrix`  | expression array, single or multi dimensional, square matrix |
|`type` | table entry type |
|`value` | expression value |

### Return values

The return value is the expression that the ArrayTranspose  represents.

## Description

**ArrayTranspose** represents transpose of square matrix, where row are stored
in column values and column values are store in rows.

## Types

Only accepts integers.

## Examples

```fortran
integer :: a(3, 4), b(4, 3)
b = transpose(a)
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            a:
                (Variable
                    1
                    a
                    Local
                    ()
                    ()
                    Default
                    (Integer 4 [((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 3 (Integer 4 [])))
                    ((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 4 (Integer 4 [])))])
                    Source
                    Public
                    Required
                    .false.
                ),
            b:
                (Variable
                    1
                    b
                    Local
                    ()
                    ()
                    Default
                    (Integer 4 [((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 4 (Integer 4 [])))
                    ((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 3 (Integer 4 [])))])
                    Source
                    Public
                    Required
                    .false.
                )

        })
    [(=
        (Var 1 b)
        (ArrayTranspose
            (Var 1 a)
            (Integer 4 [((IntegerConstant 1 (Integer 4 []))
            (IntegerConstant 4 (Integer 4 [])))
            ((IntegerConstant 1 (Integer 4 []))
            (IntegerConstant 3 (Integer 4 [])))])
            ()
        )
        ()
    )]
)

```

## See Also

