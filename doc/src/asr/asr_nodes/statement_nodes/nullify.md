# Nullify

Disassociates a pointer from a target, a `stmt` node.


## Declaration

### Syntax

```fortran
Nullify(symbol* vars)
```

### Arguments

`vars` contains pointer object to symbol variables.

### Return values

None.

## Description

**Nullify** disassociates a pointer from a target. The initial disassociation
status of a pointer is undefined. It is used to `NULLIFY` to initialize an
undefined pointer, giving it disassociated status. 

## Types


## Examples

```fortran
program nullify
implicit none

   integer, pointer :: p1, p2
   integer, target :: t1

   p1=>t1
   p2=>t1
   p1 = 1
   nullify(p1, p2)

end
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            nullify:
                (Program
                    (SymbolTable
                        2
                        {
                            p1:
                                (Variable
                                    2
                                    p1
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Pointer
                                        (Integer 4 [])
                                    )
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            p2:
                                (Variable
                                    2
                                    p2
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Pointer
                                        (Integer 4 [])
                                    )
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            t1:
                                (Variable
                                    2
                                    t1
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
                    nullify
                    []
                    [(=>
                        (Var 2 p1)
                        (Var 2 t1)
                    )
                    (=>
                        (Var 2 p2)
                        (Var 2 t1)
                    )
                    (=
                        (Var 2 p1)
                        (IntegerConstant 1 (Integer 4 []))
                        ()
                    )
                    (Nullify
                        [2 p1
                        2 p2]
                    )]
                )

        })
    []
)

```

## See Also

[Flush](flush.md)
