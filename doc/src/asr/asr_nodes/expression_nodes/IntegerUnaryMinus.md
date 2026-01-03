# IntegerUnaryMinus

Uniry minus operator as the second operand of binary arithmetic operators, a
`expr` node.

## Declaration

### Syntax

```fortran
IntegerUnaryMinus(expr arg, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`arg` |  expression arguments |
|`type`| table entry type |
|`value`| expression value|

### Return values

The return value is the expression that the IntegerUnaryMinus represents.

## Description

**IntegerUnaryMinus** represents integer unary minus operand of binary
arithmetic operators, to be used without parantheses.

Example : `a = b * -c`

## Types

Only accepts integers.

## Examples

```fortran
-1
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {

        })
    [(IntegerUnaryMinus
        (IntegerConstant 1 (Integer 4 []))
        (Integer 4 [])
        (IntegerConstant -1 (Integer 4 []))
    )]
)

```

## See Also

