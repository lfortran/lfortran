# ArrayMatMul

Array matrix multiplication.

## Declaration

### Syntax

```fortran
ArrayMatMul(expr matrix_a, expr matrix_b, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`matrix_a` | matrix first |
|`matrix_b` | second matrix |
|`type` | table entry type |
|`value | expression value |`

### Return values

The return value is the expression that the ArrayMatMul represents.

## Description

**ArrayMatMul** represents array matrix multiplication.

## Types

Only accepts integers.

## Examples

```fortran
integer :: a(3, 4)
real :: b(4, 3), cmat(3, 3)
cmat = matmul(a, b)
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            a:
                (Variable
                    1
                    a
                    Local
                    ()
                    ()
                    Default
                    (Integer 4 [((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 3 (Integer 4 [])))
                    ((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 4 (Integer 4 [])))])
                    Source
                    Public
                    Required
                    .false.
                ),
            b:
                (Variable
                    1
                    b
                    Local
                    ()
                    ()
                    Default
                    (Real 4 [((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 4 (Integer 4 [])))
                    ((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 3 (Integer 4 [])))])
                    Source
                    Public
                    Required
                    .false.
                ),
            cmat:
                (Variable
                    1
                    cmat
                    Local
                    ()
                    ()
                    Default
                    (Real 4 [((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 3 (Integer 4 [])))
                    ((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 3 (Integer 4 [])))])
                    Source
                    Public
                    Required
                    .false.
                )

        })
    [(=
        (Var 1 cmat)
        (ArrayMatMul
            (Var 1 a)
            (Var 1 b)
            (Real 4 [((IntegerConstant 1 (Integer 4 []))
            (IntegerConstant 3 (Integer 4 [])))
            ((IntegerConstant 1 (Integer 4 []))
            (IntegerConstant 3 (Integer 4 [])))])
            ()
        )
        ()
    )]
)

```

## See Also

