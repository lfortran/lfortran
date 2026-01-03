# IntegerBinOp

Integer Binary Operation expression type. An **expr** node.

## Declaration

### Syntax

```fortran
IntegerBinOp(expr left, binop op, expr right, ttype type, expr? value)
```

### Arguments

`left` and `right` represent expression on the left and right side of operator
`op`. `type` represents table entry type and `value` represents expression.

### Return values

The return value is the expression that the IntegerBinOp represents.

## Description

**IntegerBinOp** represents integer binary operation expression type. It is an
ASR expr node. ASR has multiple binary expression types, for example: for Integer,
real, complex, and logical.

If an binary expression is applying binary operator on integer operands,
`IntegerBinOp` is called which then creates LFortran expression `EXPR` using,
`ASR::make_IntegerBinOp_t` method call.

The binary operations accept two arguments of the same type. **IntegerBinOp**
only accepts integers.

## Types

Only accepts integers.

## Examples

Following example code creates LFortran expression from ASR's `IntegerBinOp`:

```fortran
(2+3)*5
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(IntegerBinOp
        (IntegerBinOp
            (IntegerConstant 2 (Integer 4 []))
            Add
            (IntegerConstant 3 (Integer 4 []))
            (Integer 4 [])
            (IntegerConstant 5 (Integer 4 []))
        )
        Mul
        (IntegerConstant 5 (Integer 4 []))
        (Integer 4 [])
        (IntegerConstant 25 (Integer 4 []))
    )]
)
```

## See Also

[RealBinOp](), [ComplexBinOp](), [LogicalBinOp]()
