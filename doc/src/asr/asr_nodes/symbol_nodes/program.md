# Program

Program symbol, a **symbol** node.

## Declaration

### Syntax

```fortran
Program(symbol_table symtab, identifier name, identifier* dependencies,
	    stmt* body)
```

### Arguments

`symtab` the symbol table of the program.

`name` the name of the program.

`dependencies` the module names that the program depends on.

`body` the list of statements that the program contains.

### Return values

None.

## Description

ASR pass transforms (in-place) the ASR tree and wraps all global
statements and expressions into a program.

## Types


## Examples

```fortran
program expr2
integer :: x
x = (2+3)*5
print *, x
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            expr2:
                (Program
                    (SymbolTable
                        2
                        {
                            x:
                                (Variable
                                    2
                                    x
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
                    expr2
                    []
                    [(=
                        (Var 2 x)
                        (IntegerBinOp
                            (IntegerBinOp
                                (IntegerConstant 2 (Integer 4 []))
                                Add
                                (IntegerConstant 3 (Integer 4 []))
                                (Integer 4 [])
                                (IntegerConstant 5 (Integer 4 []))
                            )
                            Mul
                            (IntegerConstant 5 (Integer 4 []))
                            (Integer 4 [])
                            (IntegerConstant 25 (Integer 4 []))
                        )
                        ()
                    )
                    (Print
                        ()
                        [(Var 2 x)]
                        ()
                        ()
                    )]
                )

        })
    []
)

```
## See Also

[Module](Module.md), [Function](Function.md).
