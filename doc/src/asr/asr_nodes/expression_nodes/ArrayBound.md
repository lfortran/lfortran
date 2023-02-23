# ArrayBound

Array upper and lower bound.

## Declaration

### Syntax

```fortran
ArrayBound(expr v, expr? dim, ttype type, arraybound bound,
                 expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`v`| expression |
|`dim`| dimension |
|`type` | table entry type |
|`bound` | array bound |
|`value` | expression value |

### Return values

The return value is the expression that the ArrayBound represents.

## Description

**ArrayBound** represents bounds of array. It can be upper or lower bound.

## Types

Only accepts integers.

## Examples

```fortran
integer :: a(2:5, 3:9, 7)
print *, lbound(a, 1), lbound(a, 2), lbound(a, 3)
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
                    (Integer 4 [((IntegerConstant 2 (Integer 4 []))
                    (IntegerConstant 4 (Integer 4 [])))
                    ((IntegerConstant 3 (Integer 4 []))
                    (IntegerConstant 7 (Integer 4 [])))
                    ((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 7 (Integer 4 [])))])
                    Source
                    Public
                    Required
                    .false.
                )

        })
    [(Print
        ()
        [(ArrayBound
            (Var 1 a)
            (IntegerConstant 1 (Integer 4 []))
            (Integer 4 [])
            LBound
            ()
        )
        (ArrayBound
            (Var 1 a)
            (IntegerConstant 2 (Integer 4 []))
            (Integer 4 [])
            LBound
            ()
        )
        (ArrayBound
            (Var 1 a)
            (IntegerConstant 3 (Integer 4 []))
            (Integer 4 [])
            LBound
            ()
        )]
        ()
        ()
    )]
)

```

## See Also

