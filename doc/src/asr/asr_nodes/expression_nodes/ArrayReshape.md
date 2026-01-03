# ArrayReshape

Function to reshape an array.

## Declaration

### Syntax

```fortran
ArrayReshape(expr array, expr shape, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`array`  | array expression |
|`shape` | integer value and an array of rank one, positive or zero |
|`type` | table entry type |
|`value`| expression value|

### Return values

The return value is the expression that the ArrayReshape represents.

## Description

**ArrayReshape** reshape an array. If necessary, the new array may be padded
with elements from PAD or permuted as defined by ORDER.

## Types

Only accepts integers.

## Examples

```fortran
real(8), intent(in) :: a(:, :)
real(8) :: b(256)
integer :: newshape(1)
newshape(1) = 256
b = reshape(a, newshape)
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
                    In
                    ()
                    ()
                    Default
                    (Real 8 [(()
                    ())
                    (()
                    ())])
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
                    (Real 8 [((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 256 (Integer 4 [])))])
                    Source
                    Public
                    Required
                    .false.
                ),
            newshape:
                (Variable
                    1
                    newshape
                    Local
                    ()
                    ()
                    Default
                    (Integer 4 [((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 1 (Integer 4 [])))])
                    Source
                    Public
                    Required
                    .false.
                )

        })
    [(=
        (ArrayItem
            (Var 1 newshape)
            [(()
            (IntegerConstant 1 (Integer 4 []))
            ())]
            (Integer 4 [])
            ()
        )
        (IntegerConstant 256 (Integer 4 []))
        ()
    )
    (=
        (Var 1 b)
        (ArrayReshape
            (Var 1 a)
            (Var 1 newshape)
            (Real 8 [(()
            ())])
            ()
        )
        ()
    )]
)

```

## See Also

