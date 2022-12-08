# SubroutineCall

SubroutineCall/FunctionCall to store the actual final resolved subroutine or
function (`name`) member, a `stmt` node.

## Declaration

### Syntax

```fortran
SubroutineCall(symbol name, symbol? original_name, call_arg* args, expr? dt)
```

### Arguments

`name` contains symbol name.
`original_name` contains name present in program unit.
`args` contains arguments passed to subroutine call.
`dt` contains expression for variable name, array name, an aseterist e.t.c.

### Return values

None.

## Description

**SubroutineCall** stores the actual final resolved subroutine or function (`name`
member). They also store the original symbol (`original_name`), which can be one
of null, GenericProcedure or ExternalSymbol.

## Types

Not Applicable.

## Examples

```fortran
program Subroutine_Call
implicit none
integer(4) :: from, to
from = 10
to = 4
call mvbits(from, 2, 2, to, 0)
if (from /= 10) error stop
end
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            iso_fortran_env:
                (IntrinsicModule lfortran_intrinsic_iso_fortran_env),
            lfortran_intrinsic_bit:
                (IntrinsicModule lfortran_intrinsic_bit),
            subroutine_call:
                (Program
                    (SymbolTable
                        2
                        {
                            from:
                                (Variable
                                    2
                                    from
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
                            mvbits:
                                (ExternalSymbol
                                    2
                                    mvbits
                                    4 mvbits
                                    lfortran_intrinsic_bit
                                    []
                                    mvbits
                                    Private
                                ),
                            mvbits@mvbits32:
                                (ExternalSymbol
                                    2
                                    mvbits@mvbits32
                                    4 mvbits32
                                    lfortran_intrinsic_bit
                                    []
                                    mvbits32
                                    Private
                                ),
                            to:
                                (Variable
                                    2
                                    to
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Integer 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                )

                        })
                    subroutine_call
                    []
                    [(=
                        (Var 2 from)
                        (IntegerConstant 10 (Integer 4 []))
                        ()
                    )
                    (=
                        (Var 2 to)
                        (IntegerConstant 4 (Integer 4 []))
                        ()
                    )
                    (SubroutineCall
                        2 mvbits@mvbits32
                        2 mvbits
                        [((Var 2 from))
                        ((IntegerConstant 2 (Integer 4 [])))
                        ((IntegerConstant 2 (Integer 4 [])))
                        ((Var 2 to))
                        ((IntegerConstant 0 (Integer 4 [])))]
                        ()
                    )
                    (If
                        (IntegerCompare
                            (Var 2 from)
                            NotEq
                            (IntegerConstant 10 (Integer 4 []))
                            (Logical 4 [])
                            ()
                        )
                        [(ErrorStop
                            ()
                        )]
                        []
                    )]
                )

        })
    []
)

```

## See Also
