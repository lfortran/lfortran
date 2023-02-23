# ImpliedDoLoop

Implied Do Loop expression.

## Declaration

### Syntax

```fortran
ImpliedDoLoop(expr* values, expr var, expr start, expr end,
                    expr? increment, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
| `values` | expression value |
| `var`    | expression variables |
| `start`  | expression start |
| `end`    | expression end |
| `increment | expression increment |
| `type` | table entry type |
| `value` | expression value |

### Return values

The return value is the expression that the Implied Do Loop represents.

## Description

**ImpliedDoLoop** represents implied do loop expression. The simples and most
efficient way to read or write all elements of an array is to put its name,
unsubscripted, in the data-transfer list.

An `ImpliedDoLoop` allows the elements to be transferred selectively or in some
non-standard order. The rules for an implied-DO are similar to that of an
ordinary DO-loop but the loop forms a single item in the data-transfer list.

## Types

Not applicable.

## Examples

```fortran
program implied_do_loop
	integer :: j
	integer :: a(10)=(/(j,j=1,10)/)
	print*, (a(j),j=1,10)
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            intrinsics:
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
                                    (Integer 4 [((IntegerUnaryMinus
                                        (IntegerConstant 1 (Integer 4 []))
                                        (Integer 4 [])
                                        (IntegerConstant -1 (Integer 4 []))
                                    )
                                    (IntegerConstant 3 (Integer 4 [])))
                                    ((IntegerUnaryMinus
                                        (IntegerConstant 1 (Integer 4 []))
                                        (Integer 4 [])
                                        (IntegerConstant -1 (Integer 4 []))
                                    )
                                    (IntegerConstant 4 (Integer 4 [])))])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            shape:
                                (ExternalSymbol
                                    2
                                    shape
                                    4 shape
                                    lfortran_intrinsic_builtin
                                    []
                                    shape
                                    Private
                                )

                        })
                    intrinsics
                    []
                    [(Print
                        ()
                        [(FunctionCall
                            2 shape
                            ()
                            [((Var 2 a))]
                            (Integer 4 [])
                            ()
                            ()
                        )]
                        ()
                        ()
                    )
                    (Print
                        ()
                        [(ArraySize
                            (FunctionCall
                                2 shape
                                ()
                                [((IntegerConstant 42 (Integer 4 [])))]
                                (Integer 4 [])
                                ()
                                ()
                            )
                            ()
                            (Integer 4 [])
                            (IntegerConstant 1 (Integer 4 []))
                        )]
                        ()
                        ()
                    )]
                ),
            lfortran_intrinsic_builtin:
                (IntrinsicModule lfortran_intrinsic_builtin)

        })
    []
)

```

## See Also

