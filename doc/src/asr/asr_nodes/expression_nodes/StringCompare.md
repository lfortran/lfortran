# StringCompare

String comparison.

## Declaration

### Syntax

```fortran
StringCompare(expr left, cmpop op, expr right, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`left`  | left side of comparison operator |
|`right` | right side of comparison operator |
|`type` | table entry type |
| `value` | expression value|
|`op` | operand |

### Return values

The return value is the expression that the StringCompare represents.

## Description

**StringCompare** represents string comparison expression type. Comparisons are:

1. lexicographically less than, or less than equal to
2. lexicographically greater than, or greater than or equal to
3. Equal to
4. Not equal to

## Types

Only accepts strings.

## Examples

```fortran
"aaa" > "bbb"
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(StringCompare
        (StringConstant
            "aaa"
            (Character 1 3 () [])
        )
        Gt
        (StringConstant
            "bbb"
            (Character 1 3 () [])
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

