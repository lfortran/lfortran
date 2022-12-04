# ArrayItem

Array item, value stored at each index.

## Declaration

### Syntax

```fortran
ArrayItem(expr v, array_index* args, ttype type, arraystorage storage_format, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`v` | expression |
|`args` | array indeces |
|`type` | table entry type |
|`storage_format` | array storage format |
|`value` | expression value |

### Return values

The return value is the expression that the ArrayItem represents.

## Description

**ArrayItem** represents Array Item.

## Types

Only accepts array indexes.

## Examples

```fortran
integer :: a(2)
a(0) = 0
a(1) = 1
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
                    (IntegerConstant 2 (Integer 4 [])))])
                    Source
                    Public
                    Required
                    .false.
                )

        })
    [(=
        (ArrayItem
            (Var 1 a)
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
            (Var 1 a)
            [(()
            (IntegerConstant 1 (Integer 4 []))
            ())]
            (Integer 4 [])
            ()
        )
        (IntegerConstant 1 (Integer 4 []))
        ()
    )]
)

```

## See Also

[ArrayConstant](ArrayConstant.md)
