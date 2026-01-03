# RealBinOp

Real Binary Operation expression type. An **expr** node.

## Declaration

### Syntax

```fortran
RealBinOp(expr left, binop op, expr right, ttype type, expr? value)
```

### Arguments

`left` and `right` represent expression on the left and right side of operator
`op`. `type` represents table entry type and `value` represents expression.

### Return values

The return value is the expression that the RealBinOp represents.

## Description

**RealBinOp** represents real binary operation expression type. It is an
ASR expr node. ASR has multiple binary expression types, for example: for Integer,
real, complex, and logical.

The binary operations accept two arguments of the same type.

## Types

Only accepts real constants, real exponents, floating point values.

## Examples

```fortran
(2.1+3.1)*5.1
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(RealBinOp
        (RealBinOp
            (RealConstant
                2.100000
                (Real 4 [])
            )
            Add
            (RealConstant
                3.100000
                (Real 4 [])
            )
            (Real 4 [])
            (RealConstant
                5.200000
                (Real 4 [])
            )
        )
        Mul
        (RealConstant
            5.100000
            (Real 4 [])
        )
        (Real 4 [])
        (RealConstant
            26.520000
            (Real 4 [])
        )
    )]
)

```

## See Also

[IntegerBinOp](IntegerBinOp.md), [ComplexBinOp](ComplexBinOp.md),
[LogicalBinOp](LogicalBinOp.md)
