# CPtrToPointer

Associtates a data pointer with the target of a C pointer and specifies its
shape, a `stmt` node.

## Declaration

### Syntax

```fortran
CPtrToPointer(expr cptr, expr ptr, expr? shape)
```

### Arguments

`cptr` contains the C address of an interoperable data entity, or the result of
a reference to function `C_LOC` with a noninteroperable argument. If the value
of `cptr` is the C address of a Fortran varible, it must have the `TARGET`
attribute.

`ptr` contains data pointer. If it is an array, `shape` must be specified.

`shape` is of type integer and rank one. Its size equals the rank of `ptr`.

### Return values

None.

## Description

**CPtrToPointer** is the statement node for conversion from C pointer to data
pointer.

If the `ptr` is an array, it has the shape specified by `shape` and each lower
bound is 1.

## Types

`cptr` and `ptr` should be of type pointer holding an address.
`shape` must be of type integer.

## Examples

```fortran
program bindc
use iso_c_binding, only: c_loc, c_ptr, c_f_pointer
type(c_ptr) :: queries
integer :: idx = 1
integer(2), pointer :: x
integer(2), target :: y
call c_f_pointer(queries, x)
print *, c_loc(x), queries
x => y
print *, c_loc(x), c_loc(y)
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            bindc:
                (Program
                    (SymbolTable
                        2
                        {
                            c_f_pointer:
                                (ExternalSymbol
                                    2
                                    c_f_pointer
                                    4 c_f_pointer
                                    lfortran_intrinsic_iso_c_binding
                                    []
                                    c_f_pointer
                                    Public
                                ),
                            c_loc:
                                (ExternalSymbol
                                    2
                                    c_loc
                                    4 c_loc
                                    lfortran_intrinsic_iso_c_binding
                                    []
                                    c_loc
                                    Public
                                ),
                            c_ptr:
                                (ExternalSymbol
                                    2
                                    c_ptr
                                    4 c_ptr
                                    lfortran_intrinsic_iso_c_binding
                                    []
                                    c_ptr
                                    Public
                                ),
                            idx:
                                (Variable
                                    2
                                    idx
                                    Local
                                    (IntegerConstant 1 (Integer 4 []))
                                    ()
                                    Save
                                    (Integer 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            queries:
                                (Variable
                                    2
                                    queries
                                    Local
                                    ()
                                    ()
                                    Default
                                    (CPtr)
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
                                    (Pointer
                                        (Integer 2 [])
                                    )
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            y:
                                (Variable
                                    2
                                    y
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Integer 2 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                )

                        })
                    bindc
                    [iso_c_binding]
                    [(CPtrToPointer
                        (Var 2 queries)
                        (Var 2 x)
                        ()
                    )
                    (Print
                        ()
                        [(PointerToCPtr
                            (Var 2 x)
                            (CPtr)
                            ()
                        )
                        (Var 2 queries)]
                        ()
                        ()
                    )
                    (=>
                        (Var 2 x)
                        (Var 2 y)
                    )
                    (Print
                        ()
                        [(PointerToCPtr
                            (Var 2 x)
                            (CPtr)
                            ()
                        )
                        (PointerToCPtr
                            (GetPointer
                                (Var 2 y)
                                (Pointer
                                    (Integer 2 [])
                                )
                                ()
                            )
                            (CPtr)
                            ()
                        )]
                        ()
                        ()
                    )]
                ),
            iso_c_binding:
                (IntrinsicModule lfortran_intrinsic_iso_c_binding),
            lfortran_intrinsic_builtin:
                (IntrinsicModule lfortran_intrinsic_builtin)

        })
    []
)

```

## See Also
