# ArraySize

Size of array.

## Declaration

### Syntax

```fortran
ArraySize(expr v, expr? dim, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`v` | expression |
|`dim` | expression dimension |
|`type` | table entry type |
|`value` | expression value |

### Return values

The return value is the expression that the ArraySize represents.

## Description

**ArraySize** represents size of array.

## Types

Only accepts integers.

## Examples

```fortran
integer :: a(3)
integer(8) :: size_a8
size_a8 = size(a, kind=8)
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
                    (IntegerConstant 3 (Integer 4 [])))])
                    Source
                    Public
                    Required
                    .false.
                ),
            size_a8:
                (Variable
                    1
                    size_a8
                    Local
                    ()
                    ()
                    Default
                    (Integer 8 [])
                    Source
                    Public
                    Required
                    .false.
                )

        })
    [(=
        (Var 1 size_a8)
        (ArraySize
            (Var 1 a)
            ()
            (Integer 8 [])
            (IntegerConstant 3 (Integer 8 []))
        )
        ()
    )]
)

```

## See Also

