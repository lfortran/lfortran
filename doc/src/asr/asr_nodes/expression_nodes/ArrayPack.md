# ArrayPack

Pack an array into an array of rank one.

## Declaration

### Syntax

```fortran
 ArrayPack(expr array, expr mask, expr? vector, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`array`  | expression array |
|`mask` | array mask |
|`vector` | vector expression |
|`type` | table entry type |
|`value`| expression value |

### Return values

The return value is the expression that the ArrayPack represents.

## Description

**ArrayPack** stores the elements of array in an array or rank one.
The beginning of the resulting array is made up of elements whose MASK equals TRUE.
Afterwards, positions are filled with elements taken from VECTOR.

The result is an array of rank one and the same type as that of ARRAY.
If VECTOR is present, the result size is that of VECTOR, the number of TRUE
values in MASK otherwise.

## Types

Only accepts integers.

## Examples

```fortran
integer :: m(6), p(2)
m = [ 1, 0, 0, 0, 5, 0 ]
p = pack(m, m /= 0)
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            m:
                (Variable
                    1
                    m
                    Local
                    ()
                    ()
                    Default
                    (Integer 4 [((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 6 (Integer 4 [])))])
                    Source
                    Public
                    Required
                    .false.
                ),
            p:
                (Variable
                    1
                    p
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
        (Var 1 m)
        (ArrayConstant
            [(IntegerConstant 1 (Integer 4 []))
            (IntegerConstant 0 (Integer 4 []))
            (IntegerConstant 0 (Integer 4 []))
            (IntegerConstant 0 (Integer 4 []))
            (IntegerConstant 5 (Integer 4 []))
            (IntegerConstant 0 (Integer 4 []))]
            (Integer 4 [((IntegerConstant 1 (Integer 4 []))
            (IntegerConstant 6 (Integer 4 [])))])
        )
        ()
    )
    (=
        (Var 1 p)
        (ArrayPack
            (Var 1 m)
            (IntegerCompare
                (Var 1 m)
                NotEq
                (IntegerConstant 0 (Integer 4 []))
                (Logical 4 [])
                ()
            )
            ()
            (Integer 4 [((IntegerConstant 1 (Integer 4 []))
            (ArraySize
                (IntegerCompare
                    (Var 1 m)
                    NotEq
                    (IntegerConstant 0 (Integer 4 []))
                    (Logical 4 [])
                    ()
                )
                ()
                (Integer 4 [])
                ()
            ))])
            ()
        )
        ()
    )]
)

```

## See Also

