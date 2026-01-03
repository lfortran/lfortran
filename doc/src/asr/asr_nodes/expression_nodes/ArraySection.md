# ArraySection

Section of Array.

## Declaration

### Syntax

```fortran
ArraySection(expr v, array_index* args, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`v`  | expression |
|`args` | array index arguments |
|`type` | table entry type |
|`value` | expression value |

### Return values

The return value is the expression that the ArraySection represents.

## Description

**ArraySection** represents section of the array.

## Types

Only accepts section of arrays.

## Examples

```fortran
integer, dimension(3) :: x
x(0) = 0
x(1) = 1
x(2) = 2
print *, x(1:)
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
                    ()
                    ()
                    Default
                    (Integer 4 [((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 3 (Integer 4 [])))])
                    Source
                    Public
                    Required
                    .false.
                )

        })
    [(=
        (ArrayItem
            (Var 1 x)
            [(()
            (IntegerConstant 0 (Integer 4 []))
            ())]
            (Integer 4 [])
            ()
        )
        (IntegerConstant 0 (Integer 4 []))
        ()
    )
    (=
        (ArrayItem
            (Var 1 x)
            [(()
            (IntegerConstant 1 (Integer 4 []))
            ())]
            (Integer 4 [])
            ()
        )
        (IntegerConstant 1 (Integer 4 []))
        ()
    )
    (=
        (ArrayItem
            (Var 1 x)
            [(()
            (IntegerConstant 2 (Integer 4 []))
            ())]
            (Integer 4 [])
            ()
        )
        (IntegerConstant 2 (Integer 4 []))
        ()
    )
    (Print
        ()
        [(ArraySection
            (Var 1 x)
            [((IntegerConstant 1 (Integer 4 []))
            ()
            (IntegerConstant 1 (Integer 4 [])))]
            (Integer 4 [((IntegerConstant 1 (Integer 4 []))
            (IntegerConstant 3 (Integer 4 [])))])
            ()
        )]
        ()
        ()
    )]
)

```

## See Also

