# LogicalBinOp

Logical Binary Operation expression type. An **expr** node.

## Declaration

### Syntax

```fortran
LogicalBinOp(expr left, binop op, expr right, ttype type, expr? value)
```

### Arguments

`left` and `right` represent expression on the left and right side of operator
`op`. `type` represents table entry type and `value` represents expression.

### Return values

The return value is the expression that the LogicalBinOp represents.

## Description

**LogicalBinOp** represents logical binary operation expression type. It is an
ASR expr node. ASR has multiple binary expression types, for example: for Integer,
real, complex, and logical.

The binary operations accept two arguments of the same type.

## Types

Only accepts `.TRUE.` and `.FALSE.`

## Examples

```fortran
bgt(10, 4) .neqv. .true.
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            bgt:
                (ExternalSymbol
                    1
                    bgt
                    3 bgt
                    lfortran_intrinsic_bit
                    []
                    bgt
                    Private
                ),
            bgt@bgt32:
                (ExternalSymbol
                    1
                    bgt@bgt32
                    3 bgt32
                    lfortran_intrinsic_bit
                    []
                    bgt32
                    Private
                ),
            iso_fortran_env:
                (IntrinsicModule lfortran_intrinsic_iso_fortran_env),
            lfortran_intrinsic_bit:
                (IntrinsicModule lfortran_intrinsic_bit)

        })
    [(LogicalBinOp
        (FunctionCall
            1 bgt@bgt32
            1 bgt
            [((IntegerConstant 10 (Integer 4 [])))
            ((IntegerConstant 4 (Integer 4 [])))]
            (Logical 4 [])
            ()
            ()
        )
        NEqv
        (LogicalConstant
            .true.
            (Logical 4 [])
        )
        (Logical 4 [])
        ()
    )]
)

```

## See Also

[IntegerBinOp](IntegerBinOp.md), [ComplexBinOp](ComplexBinOp.md),
[RealBinOp](RealBinOp.md)
