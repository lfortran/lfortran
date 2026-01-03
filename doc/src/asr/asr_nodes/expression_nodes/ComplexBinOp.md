# ComplexBinOp

Complex Binary Operation expression type. An **expr** node.

## Declaration

### Syntax

```fortran
ComplexBinOp(expr left, binop op, expr right, ttype type, expr? value)
```

### Arguments

`left` and `right` represent expression on the left and right side of operator
`op`. `type` represents table entry type and `value` represents expression.

### Return values

The return value is the expression that the ComplexBinOp represents.

## Description

**ComplexBinOp** represents complex binary operation expression type. It is an
ASR expr node. ASR has multiple binary expression types, for example: for Integer,
real, complex, and logical.

The binary operations accept two arguments of the same type.

## Types

Only accepts real constants, real exponents, floating point values, integer, as
real and imaginary part of the complex constant.

## Examples

```fortran
(2.1, 3.1) + 5.1
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(ComplexBinOp
        (ComplexConstructor
            (RealConstant
                2.100000
                (Real 4 [])
            )
            (RealConstant
                3.100000
                (Real 4 [])
            )
            (Complex 4 [])
            (ComplexConstant
                2.100000
                3.100000
                (Complex 4 [])
            )
        )
        Add
        (Cast
            (RealConstant
                5.100000
                (Real 4 [])
            )
            RealToComplex
            (Complex 4 [])
            (ComplexConstant
                5.100000
                0.000000
                (Complex 4 [])
            )
        )
        (Complex 4 [])
        (ComplexConstant
            7.200000
            3.100000
            (Complex 4 [])
        )
    )]
)
```

## See Also

[IntegerBinOp](IntegerBinOp.md), [RealBinOp](RealBinOp.md),
[LogicalBinOp](LogicalBinOp.md)
