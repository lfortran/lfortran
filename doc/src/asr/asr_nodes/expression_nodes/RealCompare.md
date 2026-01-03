# RealCompare

Real comparison `expr` ASR node.

## Declaration

### Syntax

```fortran
RealCompare(expr left, cmpop op, expr right, ttype type, expr? value)
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

The return value is the expression that the RealCompare represents.

## Description

**RealCompare** represents real floating point comparison operation.

Comparison operation can be:

1. Less than
2. Greater than
3. Less than or equal to
4. Greater than or equal to
5. Equal to

## Types

Only accepts real constants, real exponents, floating point values.

## Examples

```fortran
2.1 > 1.
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(RealCompare
        (RealConstant
            2.100000
            (Real 4 [])
        )
        Gt
        (Cast
            (IntegerConstant 1 (Integer 4 []))
            IntegerToReal
            (Real 4 [])
            (RealConstant
                1.000000
                (Real 4 [])
            )
        )
        (Logical 4 [])
        (LogicalConstant
            .true.
            (Logical 4 [])
        )
    )]
)

```

## See Also

[IntegerCompare](IntegerCompare.md)
