# kinds

The `kind` member selects the kind of a given type.

## Declaration

### Syntax

```fortran
Integer
Real
Complex
Character
Logical
kind
```
### Arguments

None.

### Return values

None.

## Description

**kind** denotes the kind of a given type. LFortran supports the following:

1. Integer kinds: 1 (i8), 2 (i16), 4 (i32), 8 (i64)
2. Real kinds: 4 (f32), 8 (f64)
3. Complex kinds: 4 (c32), 8 (c64)
4. Character kinds: 1 (utf8 string)
5. Logical kinds: 1, 2, 4: (boolean represented by 1, 2, 4 bytes; the default
kind is 4, just like the default integer kind, consistent with Python and Fortran:
in Python "Booleans in Python are implemented as a subclass of integers", in
Fortran the "default logical kind has the same storage size as the default integer";
we currently use kind=4 as default integer, so we also use kind=4 for the
default logical.)

## Types

LFortan supports the following types:

1. Integer kinds
2. Real kinds
3. Complex kinds
4. Character kinds
5. Logical kinds

## Examples

```fortran
program const_kind_01
integer, parameter :: sp = 4, dp = 8
real(sp), parameter :: r1 = 1.0_sp
real(dp), parameter :: r2 = 1.0_dp
real :: r3
r3 = 1.0_sp
r3 = 1.0_dp
print *, sp, dp, r1, r2, r3
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            const_kind_01:
                (Program
                    (SymbolTable
                        2
                        {
                            dp:
                                (Variable
                                    2
                                    dp
                                    Local
                                    (IntegerConstant 8 (Integer 4 []))
                                    (IntegerConstant 8 (Integer 4 []))
                                    Parameter
                                    (Integer 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            r1:
                                (Variable
                                    2
                                    r1
                                    Local
                                    (RealConstant
                                        1.000000
                                        (Real 4 [])
                                    )
                                    (RealConstant
                                        1.000000
                                        (Real 4 [])
                                    )
                                    Parameter
                                    (Real 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            r2:
                                (Variable
                                    2
                                    r2
                                    Local
                                    (RealConstant
                                        1.000000
                                        (Real 8 [])
                                    )
                                    (RealConstant
                                        1.000000
                                        (Real 8 [])
                                    )
                                    Parameter
                                    (Real 8 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            r3:
                                (Variable
                                    2
                                    r3
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Real 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            sp:
                                (Variable
                                    2
                                    sp
                                    Local
                                    (IntegerConstant 4 (Integer 4 []))
                                    (IntegerConstant 4 (Integer 4 []))
                                    Parameter
                                    (Integer 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                )

                        })
                    const_kind_01
                    []
                    [(=
                        (Var 2 r3)
                        (RealConstant
                            1.000000
                            (Real 4 [])
                        )
                        ()
                    )
                    (=
                        (Var 2 r3)
                        (Cast
                            (RealConstant
                                1.000000
                                (Real 8 [])
                            )
                            RealToReal
                            (Real 4 [])
                            (RealConstant
                                1.000000
                                (Real 4 [])
                            )
                        )
                        ()
                    )
                    (Print
                        ()
                        [(Var 2 sp)
                        (Var 2 dp)
                        (Var 2 r1)
                        (Var 2 r2)
                        (Var 2 r3)]
                        ()
                        ()
                    )]
                )

        })
    []
)
```

## See Also
