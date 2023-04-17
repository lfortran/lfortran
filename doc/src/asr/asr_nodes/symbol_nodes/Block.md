# Block

The **Block** is a `symbol` node that represents a scoped list of statements
(block).

## Declaration

### Syntax

```
Block(symbol_table symtab, identifier name, stmt* body)
```

### Arguments

| Argument Name   | Denotes                                          |
|-----------------|--------------------------------------------------|
| `parent_symtab` | the parent symbol table that contains the block  |
| `name`          | the name of the block in the parent symbol table |
| `body`          | the list of statements in the block              |

### Return values

None.

## Description

The Block node represents a scoped list of statements. In C one uses `{}` to
represent a block, in Fortran one uses `block` / `end block`. The Block
contains its own symbol table and can contain variable declarations that are
local to the block.

The Block itself is part of a symbol table and one uses the BlockCall `stmt`
node to call (enter) the block from a list of statements.

A `BlockCall` is different from a `FunctionCall` in the sense that it does
not expect/accept any arguments. It is more like an inline `Function`. Also,
the statements inside a `Block` can access the variables in the parent/caller
scope, unlike `Function` where statements cannot access variables of the caller
scope.

## Types


## Examples

An example in C is:
```c
{
    int i = 5;
    print("%d\n", i);
}
```

An example in Fortran is:

```fortran
program test_block
integer :: i
i = 5
block
    integer :: j
    j = i + 1
    print *, i, j
end block
end program
```

ASR:

```clojure
(TranslationUnit
    (SymbolTable
        1
        {
            test_block:
                (Program
                    (SymbolTable
                        2
                        {
                            block:
                                (Block
                                    (SymbolTable
                                        3
                                        {
                                            j:
                                                (Variable
                                                    3
                                                    j
                                                    []
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
                                    block
                                    [(=
                                        (Var 3 j)
                                        (IntegerBinOp
                                            (Var 2 i)
                                            Add
                                            (IntegerConstant 1 (Integer 4 []))
                                            (Integer 4 [])
                                            ()
                                        )
                                        ()
                                    )
                                    (Print
                                        ()
                                        [(Var 2 i)
                                        (Var 3 j)]
                                        ()
                                        ()
                                    )]
                                ),
                            i:
                                (Variable
                                    2
                                    i
                                    []
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
                    test_block
                    []
                    [(=
                        (Var 2 i)
                        (IntegerConstant 5 (Integer 4 []))
                        ()
                    )
                    (BlockCall
                        -1
                        2 block
                    )]
                )
        })
    []
)
```

## See Also

[symbol](symbol.md).
