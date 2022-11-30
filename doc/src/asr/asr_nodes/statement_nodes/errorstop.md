# ErrorStop

Error termination, a statement node.

## Declaration

### Syntax

```fortran
ErrorStop(expr? code)
```

### Arguments

`code` is an integer or default character constant expression and has the same
meaning as for the `stop` statement.

### Return values

May or may not return an error code.

## Description

**ErrorStop** for error termination, happens otherwise on input/output, allocation
or other such forms.

It is useful for stopping all running images or processes.

## Types

Name of variable, must be a pointer or allocatable variable.

## Examples

```fortran
program errorstop
implicit none
integer :: i

i = 0
do i = 1, 2
	print i
end do
if (i /= 2) error stop
print *, i
end
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            errorstop:
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
                                )

                        })
                    errorstop
                    []
                    [(=
                        (Var 2 i)
                        (IntegerConstant 0 (Integer 4 []))
                        ()
                    )
                    (DoLoop
                        ((Var 2 i)
                        (IntegerConstant 1 (Integer 4 []))
                        (IntegerConstant 2 (Integer 4 []))
                        ())
                        [(Print
                            (Var 2 i)
                            []
                            ()
                            ()
                        )]
                    )
                    (If
                        (IntegerCompare
                            (Var 2 i)
                            NotEq
                            (IntegerConstant 2 (Integer 4 []))
                            (Logical 4 [])
                            ()
                        )
                        [(ErrorStop
                            ()
                        )]
                        []
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

## See Also
