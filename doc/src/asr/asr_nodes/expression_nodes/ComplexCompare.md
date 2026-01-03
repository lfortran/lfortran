# ComplexCompare

Complex comparison `expr` ASR node.

## Declaration

### Syntax

```fortran
ComplexCompare(expr left, cmpop op, expr right, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`left`      | left side of the comparison operand |
|`right` | right side of the comparison operand |
|`op` | comparison operator |
|`type` | table entry type |
|`value`| expression value|

### Return values

The return value is the expression that the ComplexCompare represents.

## Description

**ComplexCompare** represents complex comparison operation.

Comparison operation can be:

1. Less than
2. Greater than
3. Less than or equal to
4. Greater than or equal to
5. Equal to

## Types

Only accepts integers, real constants, real exponents, floating point values as
real and imaginary part of complex constant.

## Examples

```fortran
(2.1, 1.1) /= 2
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(ComplexCompare
        (ComplexConstructor
            (RealConstant
                2.100000
                (Real 4 [])
            )
            (RealConstant
                1.100000
                (Real 4 [])
            )
            (Complex 4 [])
            (ComplexConstant
                2.100000
                1.100000
                (Complex 4 [])
            )
        )
        NotEq
        (Cast
            (IntegerConstant 2 (Integer 4 []))
            IntegerToComplex
            (Complex 4 [])
            ()
        )
        (Logical 4 [])
        ()
    )]
)

```

## See Also

[IntegerCompare](IntegerCompare.md), [RealCompare](RealCompare.md)
