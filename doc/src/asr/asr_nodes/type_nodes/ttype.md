# ttype

Type nodes.

## Declaration

### Syntax

```fortran
ttype
    = Integer(int kind, dimension* dims)
    | Real(int kind, dimension* dims)
    | Complex(int kind, dimension* dims)
    | Character(int kind, int len, expr? len_expr, dimension* dims)
    | Logical(int kind, dimension* dims)
    | Set(ttype type)
    | List(ttype type)
    | Tuple(ttype* type)
    | Struct(symbol derived_type, dimension* dims)
    | Enum(symbol enum_type, dimension *dims)
    | Union(symbol union_type, dimension *dims)
    | Class(symbol class_type, dimension* dims)
    | Dict(ttype key_type, ttype value_type)
    | Pointer(ttype type)
    | CPtr()
    | TypeParameter(identifier param, dimension* dims)
```

### Arguments

`kind` member selects the kind of a given type.
`dims` denotes dimension descriptor.
`expr` denotes expression.
`len` denotes length of variable allowed.
`derived_type` denotes derived type of class. `derived_type` must point to a
symbol with a symbol table.
`enum_type` denotes enumeration type.
`union_type` denotes union type.
`class_type` denotes class type.
`key_type` denotes key type in dictonary type.
`value_type` denotes value type in dictionary type.
`param` denotes identifier or variable's.


### Return values

None.

## Description

`ttype` denotes type of variables supported by LFortran. It consists of:

1. `Integer` denotes integer type.
2. `Real` denotes real type.
3. `Complex` denotes complex type.
4. `Character` denotes character type.
5. `Logical` denotes logical type.
6. `Set` denotes set type.
7. `List` denotes list type.
8. `Tuple` denotes tuple type.
9. `Struct` denotes structure type.
10. `Enum` denotes enumeration type.
11. `Union` denotes union type.
12. `Class` denotes class type.
13. `Dict` denotes dictionary type.
14. `Pointer` denotes Pointer type.
15. `CPtr` denotes C pointer type.
16. `TypeParameter` denotes type of parameter.


## Types

**ttype** defines types of variables or identifiers supported by LFortran.

## Examples

Example for `integer`:

```fortran
integer(8) :: x
x = 6
x
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            x:
                (Variable
                    1
                    x
                    Local
                    ()
                    ()
                    Default
                    (Integer 8 [])
                    Source
                    Public
                    Required
                    .false.
                )

        })
    [(=
        (Var 1 x)
        (Cast
            (IntegerConstant 6 (Integer 4 []))
            IntegerToInteger
            (Integer 8 [])
            (IntegerConstant 6 (Integer 8 []))
        )
        ()
    )
    (Var 1 x)]
)
```

Example for `derived_type`:

```fortran
program t01_derived_type
implicit none
type type_A
    integer :: i
    real :: r
end type
type(type_A) :: A
A%i = 5
A%r = 5.5
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            t01_derived_type:
                (Program
                    (SymbolTable
                        2
                        {
                            a:
                                (Variable
                                    2
                                    a
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Struct
                                        2 type_a
                                        []
                                    )
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            type_a:
                                (StructType
                                    (SymbolTable
                                        3
                                        {
                                            i:
                                                (Variable
                                                    3
                                                    i
                                                    Local
                                                    ()
                                                    ()
                                                    Default
                                                    (Integer 4 [])
                                                    Source
                                                    Public
                                                    Required
                                                    .false.
                                                ),
                                            r:
                                                (Variable
                                                    3
                                                    r
                                                    Local
                                                    ()
                                                    ()
                                                    Default
                                                    (Real 4 [])
                                                    Source
                                                    Public
                                                    Required
                                                    .false.
                                                )

                                        })
                                    type_a
                                    [i
                                    r]
                                    Source
                                    Public
                                    ()
                                )

                        })
                    t01_derived_type
                    []
                    [(=
                        (StructInstanceMember
                            (Var 2 a)
                            3 i
                            (Integer 4 [])
                            ()
                        )
                        (IntegerConstant 5 (Integer 4 []))
                        ()
                    )
                    (=
                        (StructInstanceMember
                            (Var 2 a)
                            3 r
                            (Real 4 [])
                            ()
                        )
                        (RealConstant
                            5.500000
                            (Real 4 [])
                        )
                        ()
                    )]
                )

        })
    []
)
```

Example for `complex`:

```fortran
program complex1
complex :: x
x = (3.0, 4.0)
end program
```

ASR:

```
(TranslationUnit
    (SymbolTable
        1
        {
            complex1:
                (Program
                    (SymbolTable
                        2
                        {
                            x:
                                (Variable
                                    2
                                    x
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Complex 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                )

                        })
                    complex1
                    []
                    [(=
                        (Var 2 x)
                        (ComplexConstructor
                            (RealConstant
                                3.000000
                                (Real 4 [])
                            )
                            (RealConstant
                                4.000000
                                (Real 4 [])
                            )
                            (Complex 4 [])
                            (ComplexConstant
                                3.000000
                                4.000000
                                (Complex 4 [])
                            )
                        )
                        ()
                    )]
                )

        })
    []
)
```

Example for `dimension`:

```fortran
integer :: x
dimension x(3)
```

ASR:

```
(TranslationUnit
    (SymbolTable
        1
        {
            x:
                (Variable
                    1
                    x
                    Local
                    ()
                    ()
                    Default
                    (Integer 4 [((IntegerConstant 1 (Integer 4 []))
                    (IntegerConstant 3 (Integer 4 [])))])
                    Source
                    Public
                    Required
                    .false.
                )

        })
    []
)
```
## See Also
