# Requirement

A **Requirement** is a `symbol` node for declaring generic types 
and operations associated with the types.
 
## Declaration

### Syntax

```fortran
Requirement(symbol_table symtab, identifier name, 
            identifier* args, require_instantiation* requires)
```

### Arguments

| Argument Name              | Denotes                    |
|----------------------------|----------------------------|
| `symtab`                   | symbol table of the requirement                    |
| `name`                     | name of the requirement                            |
| `args`                     | symbol names inside the requirement                |
| `require_instantiation`    | instantiating argument types through require calls |

### Return values

N/A

## Description

Generic types and their abstract methods (whose implementation are not yet known) are needed during implementation of generic functions. Requirements fills the hole here by declaring (adhoc) generic types and their associated methods. Requirements are analoguos to *typeclasses* in Haskell and *traits* in Rust.

`name` denotes the name of the requirement. In the example below, the `name` of the requirements are `semigroup` and `monoid`.

`args` denotes the parameters of the requirement. A warning is generated if there is no corresponding symbol found in the requirement's `symtab` for a given paramater. In the example below, the parameters `(T, op)` make up the `args` of the `semigroup` requirement.

`symtab` denotes the symbol table of the requirement. It contains generic types, represented by variables typed with `TypeParameter`, and *abstract* functions whose signatures may contain generic types. An error is generated if a symbol found in the `symtab` but not declared in `args`. In the example below, `semigroup`'s `symtab` contains the variable `T` with type `TypeParameter T` and the function `op` with type `TypeParameter T x TypeParameter T -> TypeParameter T`.

`require_instantiation` (`Require` statements) are calls to requirements that replace the types of the arguments with the corresponding parameters' types in the requirement. As an example the requirement `monoid` below reuses `semigroup` through `require :: semigroup(S, combine)`. Although `S` and `combine` are not declared as symbols inside `monoid`, this statement maps the typing for both `S` and `combine` with the parameter `T` and `op` in `semigroup` that defines `S` as a type parameter. As a result, `type(S)` is a valid type in `monoid` and two methods `combine` and `empty` are associated with it.

## Types

## Examples

LFortran:
```fortran
module semigroup_m
  requirement semigroup(T, op)
    type, deferred :: T
    function op(x,y) result(z)
      type(T), intent(in) :: x, y
      type(T) :: z
    end function
  end requirement
  requirement monoid(S, combine, empty)
      require :: semigroup(S, combine)
      pure function empty()
          type(S) :: empty
      end function
  end requirement
end module
```

ASR:
```
semigroup_m:
  (Module
    (SymbolTable
      2
      {
        semigroup:
          (Requirement
            (SymbolTable
                3
                {
                  op:
                    (Function
                      (SymbolTable
                        4
                        {
                          x:
                            (Variable
                              4
                              x
                              []
                              In
                              ()
                              ()
                              Default
                              (TypeParameter
                                  t
                              )
                              ()
                              Source
                              Public
                              Required
                              .false.
                            ),
                          y:
                            (Variable
                              4
                              y
                              []
                              In
                              ()
                              ()
                              Default
                              (TypeParameter
                                  t
                              )
                              ()
                              Source
                              Public
                              Required
                              .false.
                            ),
                          z:
                            (Variable
                              4
                              z
                              []
                              ReturnVar
                              ()
                              ()
                              Default
                              (TypeParameter
                                  t
                              )
                              ()
                              Source
                              Public
                              Required
                              .false.
                            )
                        })
                      op
                      (FunctionType
                        [(TypeParameter
                          t
                        )
                        (TypeParameter
                          t
                        )]
                        (TypeParameter
                          t
                        )
                        Source
                        Implementation
                        ()
                        .false.
                        .false.
                        .false.
                        .false.
                        .false.
                        []
                        .true.
                      )
                      []
                      [(Var 4 x)
                      (Var 4 y)]
                      []
                      (Var 4 z)
                      Public
                      .false.
                      .false.
                      ()
                    ),
                  t:
                    (Variable
                      3
                      t
                      []
                      In
                      ()
                      ()
                      Default
                      (TypeParameter
                        t
                      )
                      ()
                      Source
                      Public
                      Required
                      .false.
                    )
                })
              semigroup
              [t
              op]
              []
          )
      })
    semigroup_m
    []
    .false.
    .false.
  )
```

## See Also

* [Symbols](symbol.md)

* [Template](Template.md)

* [Generics](../../generics.md)