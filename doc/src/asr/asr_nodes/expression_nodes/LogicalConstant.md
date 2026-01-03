# LogicalConstant

Logical literal constant, an `expr` node.

## Declaration

### Syntax

```fortran
LogicalConstant(bool value, ttype type)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
| `value`           | value of boolean |
| `type`        | tabel entry type |

### Return values

The return value is the expression that the LogicalConstant represents.

## Description

**LogicalConstant** represents logical constant, is either logical value true
or false. The only logical constants are `.TRUE` and `.FALSE`. The period
delimiters are necessary.

The value must be in the uses 4 bytes of storage.

## Types

Only accepts TRUE or FAlSe values.

## Examples


```fortran
.TRUE.
.FALSE.
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(LogicalConstant
        .true.
        (Logical 4 [])
    )
    (LogicalConstant
        .false.
        (Logical 4 [])
    )]
)
```

## See Also

[IntegerConstant](IntegerConstant.md), [ComplexConstant](ComplexConstant.md),
[RealConstant](RealConstant.md)
