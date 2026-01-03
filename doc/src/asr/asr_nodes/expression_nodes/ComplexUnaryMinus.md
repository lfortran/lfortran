# ComplexUnaryMinus

Complex unary minus operator as the second operand of binary arithmetic operators, a
`expr` node.

## Declaration

### Syntax

```fortran
ComplexUnaryMinus(expr arg, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`arg` |  expression arguments |
|`type`| table entry type |
|`value`| expression value|

### Return values

The return value is the expression that the ComplexUnaryMinus represents.

## Description

**ComplexUnaryMinus** represents complex unary minus operand of binary
arithmetic operators, to be used without parantheses.

Example : `z = -a + ic`

## Types

Only accepts floating point values, exponents, integers.

## Examples

```fortran
(-1.2, 3)
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(ComplexConstructor
        (RealUnaryMinus
            (RealConstant
                1.200000
                (Real 4 [])
            )
            (Real 4 [])
            (RealConstant
                -1.200000
                (Real 4 [])
            )
        )
        (IntegerConstant 3 (Integer 4 []))
        (Complex 4 [])
        (ComplexConstant
            -1.200000
            3.000000
            (Complex 4 [])
        )
    )]
)

```

## See Also

[IntegerUnaryMinus](IntegerUnaryMinus.md), [RealUnaryMinus](RealUnaryMinus.md)
