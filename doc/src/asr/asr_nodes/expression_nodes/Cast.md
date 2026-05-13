# Cast

Cast to different type and/or kind.

## Declaration

### Syntax

```fortran
Cast(expr arg, cast_kind kind, ttype type, expr? value, expr? dest_struct)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|   `arg`| expression argument  |
|`kind` | the cast kind enum |
|`type` | type after the cast |
|`value`| compile time value of the result if available |
|`dest_struct`| use to get struct symbol of destination struct type |

# `dest_struct` usage

Consider below example:
```fortran
type :: base
    integer :: x
end type
type, extends(base) :: derived
    integer :: y
end type

class(base), allocatable :: var
select type(var)
    type is (derived)
        print *, var%y  ! ClassToStruct
    class is (derived)
        print *, var%y  ! ClassToClass
end select
```
Here inside select type block, `var` will get wrapped inside a Cast node. This Cast node will have `class(base)` as source type and `type(derived)` as destination type. Now in backend we need both llvm types and for struct llvm types we can't get it from just destination `ttype`, we need their symbol, so for have struct symbol of destination type, we store that symbol in `dest_struct` (i.e. in this case is `derived` symbol). Similar for `ClassToClass` casting (i.e. in above example `class(base)` to `class(derived)`)


### Return values

The return value is the expression that the Cast represents.

## Description

**Cast** represents cast to different type and/or kind.

## Types

Only accepts integer, real.

## Examples

```fortran
real :: r
integer :: i
r = 1. * 2
i = 1. * 2
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            i:
                (Variable
                    1
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
                    1
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
    [(=
        (Var 1 r)
        (RealBinOp
            (RealConstant
                1.000000
                (Real 4 [])
            )
            Mul
            (Cast
                (IntegerConstant 2 (Integer 4 []))
                IntegerToReal
                (Real 4 [])
                (RealConstant
                    2.000000
                    (Real 4 [])
                )
            )
            (Real 4 [])
            (RealConstant
                2.000000
                (Real 4 [])
            )
        )
        ()
    )
    (=
        (Var 1 i)
        (Cast
            (RealBinOp
                (RealConstant
                    1.000000
                    (Real 4 [])
                )
                Mul
                (Cast
                    (IntegerConstant 2 (Integer 4 []))
                    IntegerToReal
                    (Real 4 [])
                    (RealConstant
                        2.000000
                        (Real 4 [])
                    )
                )
                (Real 4 [])
                (RealConstant
                    2.000000
                    (Real 4 [])
                )
            )
            RealToInteger
            (Integer 4 [])
            (IntegerConstant 2 (Integer 4 []))
        )
        ()
    )]
)

```

## See Also

