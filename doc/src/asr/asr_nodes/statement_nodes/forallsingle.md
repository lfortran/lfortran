# ForAllSingle

For array assignments, a statement node.

## Declaration

### Syntax

```fortran
ForAllSingle(do_loop_head head, stmt assign_stmt)
```

### Arguments

`head` contains do loop head.
`assign_stmt` contains assignment statement.

### Return values

None.

## Description

**ForAllSingle** is for array assginment. It
- can access unusual sections,
- can use indices in RHS expression
- can use indirection (vector subscripting).

## Types

Array or vector.

## Examples

```fortran
program forall_01
implicit none
integer :: ivec(3), i

forall(i=1:3) ivec(i)=i
print *,ivec

end program forall_01
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            forall_01:
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
                            ivec:
                                (Variable
                                    2
                                    ivec
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
                    forall_01
                    []
                    [(ForAllSingle
                        ((Var 2 i)
                        (IntegerConstant 1 (Integer 4 []))
                        (IntegerConstant 3 (Integer 4 []))
                        ())
                        (=
                            (ArrayItem
                                (Var 2 ivec)
                                [(()
                                (Var 2 i)
                                ())]
                                (Integer 4 [])
                                ColMajor
                                ()
                            )
                            (Var 2 i)
                            ()
                        )
                    )
                    (Print
                        ()
                        [(Var 2 ivec)]
                        ()
                        ()
                    )]
                )

        })
    []
)
```

## See Also
