# LogicalCompare

Logical comparison `expr` ASR node.

## Declaration

### Syntax

```fortran
LogicalCompare(expr left, cmpop op, expr right, ttype type, expr? value)
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

The return value is the expression that the LogicalCompare represents.

## Description

**LogicalCompare** represents logical comparison operation.

Comparison operation can be:

1. Equal to or not equal to `.TRUE.`
2. Equal to or not equal to `.FALSE.`

## Types

Only accepts `.TRUE`, `.FALSE`.

## Examples

```fortran
.FALSE. == .TRUE.
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(LogicalCompare
        (LogicalConstant
            .false.
            (Logical 4 [])
        )
        Eq
        (LogicalConstant
            .true.
            (Logical 4 [])
        )
        (Logical 4 [])
        (LogicalConstant
            .false.
            (Logical 4 [])
        )
    )]
)

```

## See Also

[IntegerCompare](IntegerCompare.md)
