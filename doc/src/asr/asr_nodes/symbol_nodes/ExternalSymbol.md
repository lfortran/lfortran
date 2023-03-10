# ExternalSymbol

External symbol, an **ExternalSymbol** node.

## Declaration

### Syntax

```fortran
ExternalSymbol(symbol_table parent_symtab, identifier name,
        symbol external, identifier module_name, identifier* scope_names,
        identifier original_name, access access)
```

### Arguments

| Argument Name              | Denotes                    |
|----------------------------|----------------------------|
| `parent_symtab`            | the parent symbol table that contains the external symbol |
| `name`                     | the name of the external symbol |
| `external`                 | pointer to the actual symbol definition |
| `module_name`              | the name of the module the symbol is in |
| `scope_names`              | a list of names if the symbol is in a nested symbol table. For example if it is a local variable in a function `f` that is nested in function `g`, then `scope_names=[g, f]` |
| `original_name`            |  |
| `access`                   | access type `Public/Private` |

### Return values

None.

## Description

ExternalSymbol represents symbols that cannot be looked up in the current scoped symbol table. As an example, if a variable is defined in a module, but used in a nested subroutine, that is not an external symbol because it can be resolved in the current symbol table (nested subroutine) by following the parents. However if a symbol is used from a different module, then it is an external symbol, because usual symbol resolution by going to the parents will not find the definition.

## Types


## Examples

```fortran
module module_num
    integer :: my_num = 5
end module

program main
  use module_num
  print *, my_num
end program

```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            main:
                (Program
                    (SymbolTable
                        3
                        {
                            my_num:
                                (ExternalSymbol
                                    3
                                    my_num
                                    2 my_num
                                    module_num
                                    []
                                    my_num
                                    Public
                                )
                        })
                    main
                    [module_num]
                    [(Print
                        ()
                        [(Var 3 my_num)]
                        ()
                        ()
                    )]
                ),
            module_num:
                (Module
                    (SymbolTable
                        2
                        {
                            my_num:
                                (Variable
                                    2
                                    my_num
                                    []
                                    Local
                                    (IntegerConstant 5 (Integer 4 []))
                                    ()
                                    Save
                                    (Integer 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                )
                        })
                    module_num
                    []
                    .false.
                    .false.
                )
        })
    []
)
```
## See Also

[symbol](symbol.md).
