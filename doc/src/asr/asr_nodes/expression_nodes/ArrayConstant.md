# ArrayConstant

Array constant.

## Declaration

### Syntax

```fortran
ArrayConstant(expr* args, ttype type, arraystorage storage_format)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`args`     | expression arguments |
|`type` | table entry type |
|`storage_format` | array storage format |

### Return values

The return value is the expression that the Array Constant represents.

## Description

**ArrayConstant** represents array constant. Array can be one or multi dimensional.
The dimension of an array may be specified by a type specification statement
`DIMENSION`.

The value of the individual array elements of the array A may be initialized to
the values 1, 2, 3, ..., 10.

The assignment of the values of one array to another is allowed provided that both
arrays in question have the same physical dimension.

An array may be allocatable, i.e., it may be assigned memory storage during execution.

## Types

Only accepts integers, floating points as values of array indexes.

## Examples

```fortran
integer :: m(4)
m = [ 1, 0, 0, 2 ]
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            m:
                (Variable
                    1
                    m
                    Local
                    ()
                    ()
                    Default
                    (Integer 4 [((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 4 (Integer 4 [])))])
                    Source
                    Public
                    Required
                    .false.
                )

        })
    [(=
        (Var 1 m)
        (ArrayConstant
            [(IntegerConstant 1 (Integer 4 []))
            (IntegerConstant 0 (Integer 4 []))
            (IntegerConstant 0 (Integer 4 []))
            (IntegerConstant 2 (Integer 4 []))]
            (Integer 4 [((IntegerConstant 1 (Integer 4 []))
            (IntegerConstant 4 (Integer 4 [])))])
        )
        ()
    )]
)
```

## See Also

[IntegerConstant](IntegerConstant.md)
