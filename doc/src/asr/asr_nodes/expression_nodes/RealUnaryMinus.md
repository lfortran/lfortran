# RealUnaryMinus

Floating point unary minus operator as the second operand of binary arithmetic operators, a
`expr` node.

## Declaration

### Syntax

```fortran
RealUnaryMinus(expr arg, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`arg` |  expression arguments |
|`type`| table entry type |
|`value`| expression value|

### Return values

The return value is the expression that the RealUnaryMinus represents.

## Description

**RealUnaryMinus** represents real unary minus operand of binary
arithmetic operators, to be used without parantheses.

Example : `a = b.x * -c.x`

## Types

Only accepts floating point values, exponents.

## Examples

```fortran
-1.2
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(RealUnaryMinus
        (RealConstant
            1.200000
            (Real 4 [])
        )
        (Real 4 [])
        (RealConstant
            -1.200000
            (Real 4 [])
        )
    )]
)
```

## See Also

[IntegerUnaryMinus](IntegerUnaryMinus.md)
