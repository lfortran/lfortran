# Ichar

Integer code for character.

## Declaration

### Syntax

```fortran
Ichar(expr arg, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`arg`| expression argument |
|`type` | table entry type|
|`value`| value of expression |

### Return values

The return value is the expression that the Ichar represents.

## Description

**Ichar** represents integer code for character.

## Types

Only accepts characters.

## Examples

```fortran
integer :: i
i = ichar(' ')
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            i:
                (Variable
                    1
                    i
                    Local
                    ()
                    ()
                    Default
                    (Integer 4 [])
                    Source
                    Public
                    Required
                    .false.
                )

        })
    [(=
        (Var 1 i)
        (Ichar
            (StringConstant
                " "
                (Character 1 1 () [])
            )
            (Integer 4 [])
            (IntegerConstant 32 (Integer 4 []))
        )
        ()
    )]
)

```

## See Also

