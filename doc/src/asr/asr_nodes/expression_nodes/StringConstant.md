# StringConstant

A string of characters enclosed in apostrophes or quotes, an `expr` node.


## Declaration

### Syntax

```fortran
StringConstant(string s, ttype type)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`s`   | value of string |
| `type` | table entry type |

### Return values

The return value is the expression that the StringConstant represents.

## Description

**StringConstant** represents string constant.
Each character string constant appearing outside a DATA statement is followed by
a null character to ease communication with C routines.

## Types

Only accepts 1 or more characters.

## Examples

```fortran
"string"
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(StringConstant
        "string"
        (Character 1 6 () [])
    )]
)
```

## See Also

[IntegerConstant](IntegerConstant.md), [RealConstant](RealConstant.md)
