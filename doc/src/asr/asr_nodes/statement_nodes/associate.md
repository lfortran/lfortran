# Associate

Associate statement, a **statement (stmt)** node.

## Declaration

### Syntax

```fortran
Associate(expr target, expr value)
```

### Arguments

`target` contains expression target.
`value` contains expression value.

### Return values

None.

## Description

**associate** statement creates an association between an idetifier and a
variable, or the value of an expression, during the execution of that comstruct.

The idetifier specified becomes an associating entity. The name of the
associating entity is an associate name.

## Types

Identifier and variable.

## Examples

```fortran
module stdlib_string_type

contains

    elemental subroutine unused_dummy_argument(dummy)
        class(*), intent(in) :: dummy
        associate(dummy => dummy); end associate
    end subroutine unused_dummy_argument

    subroutine read_formatted(v_list)
        integer, intent(in) :: v_list(:)

        call unused_dummy_argument(v_list)

    end subroutine

end module
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            stdlib_string_type:
                (Module
                    (SymbolTable
                        2
                        {
                            read_formatted:
                                (Function
                                    (SymbolTable
                                        5
                                        {
                                            v_list:
                                                (Variable
                                                    5
                                                    v_list
                                                    In
                                                    ()
                                                    ()
                                                    Default
                                                    (Integer 4 [(()
                                                    ())])
                                                    Source
                                                    Public
                                                    Required
                                                    .false.
                                                )

                                        })
                                    read_formatted
                                    [(Var 5 v_list)]
                                    [(SubroutineCall
                                        2 unused_dummy_argument
                                        ()
                                        [((Var 5 v_list))]
                                        ()
                                    )]
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
                                ),
                            unused_dummy_argument:
                                (Function
                                    (SymbolTable
                                        3
                                        {
                                            associate_block:
                                                (AssociateBlock
                                                    (SymbolTable
                                                        6
                                                        {
                                                            dummy:
                                                                (Variable
                                                                    6
                                                                    dummy
                                                                    Local
                                                                    ()
                                                                    ()
                                                                    Default
                                                                    (Pointer
                                                                        (Class
                                                                            3 ~abstract_type
                                                                            []
                                                                        )
                                                                    )
                                                                    Source
                                                                    Private
                                                                    Required
                                                                    .false.
                                                                )

                                                        })
                                                    associate_block
                                                    [(=>
                                                        (Var 6 dummy)
                                                        (Var 3 dummy)
                                                    )]
                                                ),
                                            dummy:
                                                (Variable
                                                    3
                                                    dummy
                                                    In
                                                    ()
                                                    ()
                                                    Default
                                                    (Class
                                                        3 ~abstract_type
                                                        []
                                                    )
                                                    Source
                                                    Public
                                                    Required
                                                    .false.
                                                ),
                                            ~abstract_type:
                                                (StructType
                                                    (SymbolTable
                                                        4
                                                        {

                                                        })
                                                    ~abstract_type
                                                    []
                                                    Source
                                                    Public
                                                    ()
                                                )

                                        })
                                    unused_dummy_argument
                                    [(Var 3 dummy)]
                                    [(AssociateBlockCall
                                        3 associate_block
                                    )]
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
                    stdlib_string_type
                    []
                    .false.
                    .false.
                )

        })
    []
)

```

## See Also
