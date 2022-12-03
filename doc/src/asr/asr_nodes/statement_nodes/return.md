# Return

Return statement to return control to the calling program unit, a stmt node.

## Declaration

### Syntax

```fortran
Return()
```

### Arguments

None.

### Return values

None.

## Description

**Return** statement returns control to the calling program unit. Execution of
`RETURN` statement terminates the reference of a function or subroutine.

## Types

None.

## Examples

```fortran
subroutine example (s)
    character s* "32"
    write (*,*) s
    return
end
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            example:
                (Function
                    (SymbolTable
                        2
                        {
                            s:
                                (Variable
                                    2
                                    s
                                    Unspecified
                                    (StringConstant
                                        "32"
                                        (Character 1 2 () [])
                                    )
                                    ()
                                    Save
                                    (Character 1 1 () [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                )

                        })
                    example
                    [(Var 2 s)]
                    [(FileWrite
                        0
                        ()
                        ()
                        ()
                        ()
                        ()
                        [(Var 2 s)]
                        ()
                        ()
                    )
                    (Return)]
                    ()
                    Source
                    Public
                    Implementation
                    ()
                    .false.
                    .false.
                    .false.
                    .false.
                    .false.
                    []
                    []
                    .false.
                )

        })
    []
)

```

## See Also
