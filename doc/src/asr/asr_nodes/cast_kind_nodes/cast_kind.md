# cast_kind

Cast Kind nodes or cast kind types.

## Declaration

### Syntax

```
cast_kind
    = RealToInteger
    | IntegerToReal
    | LogicalToReal
    | RealToReal
    | IntegerToInteger
    | RealToComplex
    | IntegerToComplex
    | IntegerToLogical
    | RealToLogical
    | CharacterToLogical
    | CharacterToInteger
    | CharacterToList
    | ComplexToLogical
    | ComplexToComplex
    | ComplexToReal
    | ComplexToInteger
    | LogicalToInteger
    | RealToCharacter
    | IntegerToCharacter
    | LogicalToCharacter
    | ClassToStruct
    | ClassToClass
```

### Arguments

None.

### Return values

None.

## Description

**cast_kind** nodes or cast kind types denotes kinds to typecast one idenfier to
another.

`cast_kind` denotes the types supported for cast in LFortran.

## Types

It denotes all types supported in LFortran, which are:

1. `RealToInteger` denotes `cast_kind` of `real` to `integer` type.
2. `IntegerToReal` denotes `cast_kind` of `integer` to `real` type.
3. `LogicalToReal` denotes `cast_kind` of `logical` to `real` type.
4. `RealToReal` denotes `cast_kind` of `Real` to `Real`.
5. `IntegerToInteger` denotes `cast_kind` of `integer` to `integer`.
6. `RealToComplex` denotes `cast_kind` of `Real` to `complex`.
7. `IntegerToComplex` denotes `cast_kind` of `integer` to `complex`.
8. `IntegerToLogical` denotes `cast_kind` of `integer` to `logical`.
9. `RealToLogical` denotes `cast_kind` of `real` to	`logical`.
10. `CharacterToLogical` denotes `cast_kind` of `character` to `logical`.
11. `CharacterToInteger` denotes `cast_kind` of `character` to `integer`.
12. `CharacterToList` denotes `cast_kind` of `character` to `list`.
13. `ComplexToLogical` denotes `cast_kind` of `complex` to `logical`.
14. `ComplexToComplex` denotes `cast_kind` of `complex` to `complex`.
15. `ComplexToReal` denotes `cast_kind` of `complex` to `real`.
16. `ComplexToInteger` denotes `cast_kind` of `complex` to `integer`.
17. `LogicalToInteger` denotes `cast_kind` of `logical` to `integer`.
18. `RealToCharacter` denotes `cast_kind` of `real` to `character`.
19. `IntegerToCharacter` denotes `cast_kind` of `integer` to `character`.
20. `LogicalToCharacter` denotes `cast_kind` of `logical` to `character`.

## Examples

Example for	`RealToInteger`:

```fortran
program types_03
implicit none
real :: r
integer :: i
r = 1.5
print *, r
i = r
print *, i
end program
```

ASR:

```
(TranslationUnit
    (SymbolTable
        1
        {
            types_03:
                (Program
                    (SymbolTable
                        2
                        {
                            i:
                                (Variable
                                    2
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
                                    2
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
                    types_03
                    []
                    [(=
                        (Var 2 r)
                        (RealConstant
                            1.500000
                            (Real 4 [])
                        )
                        ()
                    )
                    (Print
                        ()
                        [(Var 2 r)]
                        ()
                        ()
                    )
                    (=
                        (Var 2 i)
                        (Cast
                            (Var 2 r)
                            RealToInteger
                            (Integer 4 [])
                            ()
                        )
                        ()
                    )
                    (Print
                        ()
                        [(Var 2 i)]
                        ()
                        ()
                    )]
                )

        })
    []
)
```

Example for `IntegerToReal`:

```fortran
program types_02
implicit none
real :: r
integer :: i
i = 1
r = 1
r = i
end program
```

ASR:

```
(TranslationUnit
    (SymbolTable
        1
        {
            types_02:
                (Program
                    (SymbolTable
                        2
                        {
                            i:
                                (Variable
                                    2
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
                                    2
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
                    types_02
                    []
                    [(=
                        (Var 2 i)
                        (IntegerConstant 1 (Integer 4 []))
                        ()
                    )
                    (=
                        (Var 2 r)
                        (Cast
                            (IntegerConstant 1 (Integer 4 []))
                            IntegerToReal
                            (Real 4 [])
                            (RealConstant
                                1.000000
                                (Real 4 [])
                            )
                        )
                        ()
                    )
                    (=
                        (Var 2 r)
                        (Cast
                            (Var 2 i)
                            IntegerToReal
                            (Real 4 [])
                            ()
                        )
                        ()
                    )]
                )

        })
    []
)
```

Example of `RealToReal`:

```fortran
program types_01
implicit none
real :: r
r = 1.0
r = 1.5
r = 1.
r = float(2)
r = dble(3)
end program
```

ASR:

```
(TranslationUnit
    (SymbolTable
        1
        {
            types_01:
                (Program
                    (SymbolTable
                        2
                        {
                            r:
                                (Variable
                                    2
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
                    types_01
                    []
                    [(=
                        (Var 2 r)
                        (RealConstant
                            1.000000
                            (Real 4 [])
                        )
                        ()
                    )
                    (=
                        (Var 2 r)
                        (RealConstant
                            1.500000
                            (Real 4 [])
                        )
                        ()
                    )
                    (=
                        (Var 2 r)
                        (RealConstant
                            1.000000
                            (Real 4 [])
                        )
                        ()
                    )
                    (=
                        (Var 2 r)
                        (Cast
                            (Cast
                                (IntegerConstant 2 (Integer 4 []))
                                IntegerToReal
                                (Real 8 [])
                                (RealConstant
                                    2.000000
                                    (Real 8 [])
                                )
                            )
                            RealToReal
                            (Real 4 [])
                            (RealConstant
                                2.000000
                                (Real 4 [])
                            )
                        )
                        ()
                    )
                    (=
                        (Var 2 r)
                        (Cast
                            (Cast
                                (IntegerConstant 3 (Integer 4 []))
                                IntegerToReal
                                (Real 8 [])
                                (RealConstant
                                    3.000000
                                    (Real 8 [])
                                )
                            )
                            RealToReal
                            (Real 4 [])
                            (RealConstant
                                3.000000
                                (Real 4 [])
                            )
                        )
                        ()
                    )]
                )

        })
    []
)
```

Example of `IntegerToLogical`:

```fortran
program logical4
    ! this program checks logical operators
    implicit none

       ! variable declaration
       logical :: a, b, c

       ! assigning values
       a = 2
       b = -1
       c = 0

       print *, a, b, c

end program logical4
```

ASR:

```
(TranslationUnit
    (SymbolTable
        1
        {
            logical4:
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
                                    (Logical 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            b:
                                (Variable
                                    2
                                    b
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Logical 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            c:
                                (Variable
                                    2
                                    c
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Logical 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                )

                        })
                    logical4
                    []
                    [(=
                        (Var 2 a)
                        (Cast
                            (IntegerConstant 2 (Integer 4 []))
                            IntegerToLogical
                            (Logical 4 [])
                            ()
                        )
                        ()
                    )
                    (=
                        (Var 2 b)
                        (Cast
                            (IntegerUnaryMinus
                                (IntegerConstant 1 (Integer 4 []))
                                (Integer 4 [])
                                (IntegerConstant -1 (Integer 4 []))
                            )
                            IntegerToLogical
                            (Logical 4 [])
                            ()
                        )
                        ()
                    )
                    (=
                        (Var 2 c)
                        (Cast
                            (IntegerConstant 0 (Integer 4 []))
                            IntegerToLogical
                            (Logical 4 [])
                            ()
                        )
                        ()
                    )
                    (Print
                        ()
                        [(Var 2 a)
                        (Var 2 b)
                        (Var 2 c)]
                        ()
                        ()
                    )]
                )

        })
    []
)
```

Example of `ComplexToComplex`:

```fortran
program complex_dp

    complex(4) :: zero
    complex(8) :: v
    complex :: x
    zero = 0.0_4
    v = (1.05_4, 1.05_4)
    x = (1.05_4, 1.05_8)
    print *, v, x, zero

end program
```

ASR:

```
(TranslationUnit
    (SymbolTable
        1
        {
            complex_dp:
                (Program
                    (SymbolTable
                        2
                        {
                            v:
                                (Variable
                                    2
                                    v
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Complex 8 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
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
                                ),
                            zero:
                                (Variable
                                    2
                                    zero
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
                    complex_dp
                    []
                    [(=
                        (Var 2 zero)
                        (Cast
                            (RealConstant
                                0.000000
                                (Real 4 [])
                            )
                            RealToComplex
                            (Complex 4 [])
                            (ComplexConstant
                                0.000000
                                0.000000
                                (Complex 4 [])
                            )
                        )
                        ()
                    )
                    (=
                        (Var 2 v)
                        (Cast
                            (ComplexConstructor
                                (RealConstant
                                    1.050000
                                    (Real 4 [])
                                )
                                (RealConstant
                                    1.050000
                                    (Real 4 [])
                                )
                                (Complex 4 [])
                                (ComplexConstant
                                    1.050000
                                    1.050000
                                    (Complex 4 [])
                                )
                            )
                            ComplexToComplex
                            (Complex 8 [])
                            ()
                        )
                        ()
                    )
                    (=
                        (Var 2 x)
                        (Cast
                            (ComplexConstructor
                                (RealConstant
                                    1.050000
                                    (Real 4 [])
                                )
                                (RealConstant
                                    1.050000
                                    (Real 8 [])
                                )
                                (Complex 8 [])
                                (ComplexConstant
                                    1.050000
                                    1.050000
                                    (Complex 8 [])
                                )
                            )
                            ComplexToComplex
                            (Complex 4 [])
                            ()
                        )
                        ()
                    )
                    (Print
                        ()
                        [(Var 2 v)
                        (Var 2 x)
                        (Var 2 zero)]
                        ()
                        ()
                    )]
                )

        })
    []
)
```

Example of `ClassToStruct/ClassToClass`:

```fortran
type :: base
    integer :: x
end type
type, extends(base) :: derived
    integer :: y
end type

class(base), allocatable :: var
select type(var)
    type is (base)
        print *, var%x  ! ClassToStruct
    class is (derived)
        print *, var%y  ! ClassToClass
end select
```

ASR:

```
~select_type_block_:
    (Block
        (SymbolTable
            5
            {
                1_base_x:
                    (ExternalSymbol
                        5
                        1_base_x
                        3 x
                        base
                        []
                        x
                        Public
                    )
            })
        ~select_type_block_
        [(Print
            (StringFormat
                ()
                [(StructInstanceMember
                    (Cast
                        (Var 2 var)
                        ClassToStruct
                        (StructType
                            [(Integer 4)]
                            []
                            .true.
                            .false.
                        )
                        ()
                        (Var 2 base)
                    )
                    5 1_base_x
                    (Integer 4)
                    ()
                )]
                FormatFortran
                (Allocatable
                    (String 1 () DeferredLength DescriptorString)
                )
                ()
            )
        )]
    ),
~select_type_block_1:
    (Block
        (SymbolTable
            6
            {
                1_derived_y:
                    (ExternalSymbol
                        6
                        1_derived_y
                        4 y
                        derived
                        []
                        y
                        Public
                    )
            })
        ~select_type_block_1
        [(Print
            (StringFormat
                ()
                [(StructInstanceMember
                    (Cast
                        (Var 2 var)
                        ClassToClass
                        (StructType
                            [(Integer 4)]
                            []
                            .false.
                            .false.
                        )
                        ()
                        (Var 2 derived)
                    )
                    6 1_derived_y
                    (Integer 4)
                    ()
                )]
                FormatFortran
                (Allocatable
                    (String 1 () DeferredLength DescriptorString)
                )
                ()
            )
        )]
    )
```

LLVM types :
```llvm
%base_class = type <{ i32 (...)**, %base* }>
%base = type { i32 }
%derived_class = type <{ i32 (...)**, %derived* }>
%derived = type { %base, i32 }
```

`ClassToStruct`:
It will be used in `type is` cases. From above example we have var to cast from `class(base)` to `type(base)` in first select type block. So in `asr_to_llvm.cpp` in `visit_Cast` we will get `%base_class**` from `cast.m_arg`(i.e. `var`) and now we get destination llvm type using `cast.dest_struct` (i.e. `%base*`), so we unwrap `%base_class**` and then bitcast data member according to destination type. Here destination type will always be struct type (i.e. c_struct).
```llvm
%31 = load %source_class*, %source_class** %var, align 8
%32 = getelementptr %source_class, %source_class* %31, i32 0, i32 1  ! get data pointer
%33 = load %source*, %source** %32, align 8
%34 = bitcast %source* %33 to %destination*
```

`ClassToClass`:
It is similar as above, here just destination type will be class type. It will be used in `class is` cases where selector variable is class type and inside `class is` block also we need to cast it to specified class type. From above example, in second select type block we have to convert `%base_class**` to `%derived_class*`. Here we just load source variable and `BitCast` to destination type as both structure are same (i.e. `{vptr, data}`). 
```llvm
%65 = load %source_class*, %source_class** %var, align 8
%66 = bitcast %source_class* %65 to %destination_class*
```

For DescriptorArray selector variable, we need to create a new temporary array descriptor of destination array type where its data pointer will be bitcasted data pointer (similar as above) of original variable. 

## See Also
