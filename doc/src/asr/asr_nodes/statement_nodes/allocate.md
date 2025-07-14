# Allocate

Allocate an allocatable variable.

## Declaration

### Syntax

```fortran
Allocate(alloc_arg* args, expr? stat, expr? errmsg, expr? source)
```

### Arguments

Input argument of `args`, `stat` of type expression, `errmsg` of type expression,
and `source` of type expression.

`args` contains the arrays to be allocated.
`stat` is a variable to receive the status integer (success/failure).
`errmsg` is variable to receive error message.
`source` contains the source location.

### Return values

None.

## Description

**allocate** allocates all arrays in `args` to the sizes specified in the
`alloc_arg`.

## Types

Only accepts integer whole number value, that can be allocated on available
heap memory.

## Examples

Following example code allocates a memory block of size 2:

```fortran
program allocate_mem
real, allocatable :: a(:)
allocate(a(3))
end program
```

ASR:
```
(TranslationUnit
    (SymbolTable
        1
        {
            allocate_mem:
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
                                    Allocatable
                                    (Real 4 [(()
                                    ())])
                                    Source
                                    Public
                                    Required
                                    .false.
                                )

                        })
                    allocate_mem
                    []
                    [(Allocate
                        [(2 a
                        [((IntegerConstant 1 (Integer 4 []))
                        (IntegerConstant 3 (Integer 4 [])))])]
                        ()
                        ()
                        ()
                    )
                    (ImplicitDeallocate
                        [2 a]
                    )]
                )

        })
    []
)

```
## See Also
