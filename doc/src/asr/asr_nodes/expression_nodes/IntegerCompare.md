# IntegerCompare

Integer comparison `expr` ASR node.

## Declaration

### Syntax

```fortran
IntegerCompare(expr left, cmpop op, expr right, ttype type, expr? value)
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

The return value is the expression that the IntegerCompare represents.

## Description

**IntegerCompare** represents integer comparison operation.

Comparison operation can be:

1. Less than
2. Greater than
3. Less than or equal to
4. Greater than or equal to
5. Equal to

## Types

Only accepts integers.

## Examples

```fortran
2 > 1
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(IntegerCompare
        (IntegerConstant 2 (Integer 4 []))
        Gt
        (IntegerConstant 1 (Integer 4 []))
        (Logical 4 [])
        (LogicalConstant
            .true.
            (Logical 4 [])
        )
    )]
)

```

## See Also

