# StringConcat

String concatenation, an `expr` node.

## Declaration

### Syntax

```fortran
StringConcat(expr left, expr right, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`left`   | left string |
|`right` | right string |
|`type` | table entry type |
|`value`| expression value |

### Return values

The return value is the expression that the StringConcat represents.

## Description

**StringConcat** represents string concatenation. Two strings can be combined
one after the other using this node.

## Types

Only accepts strings.

## Examples

```fortran
"left"//"right"
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(StringConcat
        (StringConstant
            "left"
            (Character 1 4 () [])
        )
        (StringConstant
            "right"
            (Character 1 5 () [])
        )
        (Character 1 9 () [])
        (StringConstant
            "leftright"
            (Character 1 9 () [])
        )
    )]
)

```

## See Also

