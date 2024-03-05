# Template

A **Template** is a `symbol` node for declaring generic functions.

## Declaration

### Syntax

```fortran
Template(symbol_table symtab, identifier name,
         identifier* args, require_instantiation* requires)
```

### Arguments

| Argument Name              | Denotes                    |
|----------------------------|----------------------------|
| `symtab`                   | symbol table of the template                       |
| `name`                     | name of the template                               |
| `args`                     | symbol names inside the template                   |
| `require_instantiation`    | instantiating argument types through require calls |

### Return values

## Description

Generic functions are declared within the scope of a template. In the example below, a 
generic n-times multiplication is implemented in a template. A template can contain multiple generic functions that depend on one another.

`name` denotes the name of the template. Template names are needed for specifying which generic functions will be used in the program. In the example below, the `name` of the template is `derive_semigroup`.

`args` denotes the parameters of the template. The parameters of a template represent the generic elements that need to be instantiated with concrete types and functions so that the generic functions inside the template can be used during run-time. A warning is generated if there is no corresponding symbol found in the template's `symtab` for a given parameter. In the example below, the parameters '(T, op)' make up the `args` of the `derive_semigroup` template.

`require_instantiation` (`Require` statements) are calls to requiremens that replace the types of the arguments with the corresponding parameters' types in the requirement. In the generic function `stimes` below, `a` is a variable with a generic type and `op` is a function utilizing such generic variable. To assign types to both, the statement `require :: semigroup(T, op)` maps the typing for `T` and `op` according to its signature in the `semigroup`.

## Types

## Examples

LFortran: 
```fortran
module semigroup_m
  requirement semigroup(T, op)
    ...
  end requirement

  template derive_semigroup(T, op)
    require :: semigroup(T, op)
  contains
    elemental function stimes(n, a) result(res)
      integer, intent(in) :: n
      type(T), intent(in) :: a
      type(T) :: res
      integer :: i
      res = a
      do i = 2, n
        res = op(res, a)
      end do
    end function
  end template
end semigroup
```

ASR:
```
semigroup_m:
  (Module
    (SymbolTable
      2
      {
        derive_semigroup:
          (Template
            (SymbolTable
              5
              {
                op:
                  (Function
                    (SymbolTable
                      6
                      {
                        combined:
                          (Variable
                            6
                            combined
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
                          ),
                        x:
                          (Variable
                            6
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
                            6
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
                      .true.
                      .false.
                      .false.
                      .false.
                      .false.
                      []
                      .true.
                    )
                    []
                    [(Var 6 x)
                    (Var 6 y)]
                    []
                    (Var 6 combined)
                    Public
                    .false.
                    .false.
                    ()
                  ),
                stimes:
                  (Function
                    (SymbolTable
                      7
                      {
                        a:
                          (Variable
                            7
                            a
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
                        i:
                          (Variable
                            7
                            i
                            []
                            Local
                            ()
                            ()
                            Default
                            (Integer 4)
                            ()
                            Source
                            Public
                            Required
                            .false.
                          ),
                        n:
                          (Variable
                            7
                            n
                            []
                            In
                            ()
                            ()
                            Default
                            (Integer 4)
                            ()
                            Source
                            Public
                            Required
                            .false.
                          ),
                        res:
                          (Variable
                            7
                            res
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
                    stimes
                    (FunctionType
                      [(Integer 4)
                      (TypeParameter
                        t
                      )]
                      (TypeParameter
                        t
                      )
                      Source
                      Implementation
                      ()
                      .true.
                      .false.
                      .false.
                      .false.
                      .false.
                      []
                      .false.
                    )
                    [op]
                    [(Var 7 n)
                    (Var 7 a)]
                    [(Assignment
                      (Var 7 res)
                      (Var 7 a)
                      ()
                    )
                    (DoLoop
                      ()
                      ((Var 7 i)
                      (IntegerConstant 2 (Integer 4))
                      (Var 7 n)
                      ())
                      [(Assignment
                        (Var 7 res)
                        (FunctionCall
                          5 op
                          ()
                          [((Var 7 res))
                          ((Var 7 a))]
                          (TypeParameter
                            t
                          )
                          ()
                          ()
                        )
                        ()
                      )]
                    )]
                    (Var 7 res)
                    Public
                    .false.
                    .false.
                    ()
                  ),
                t:
                  (Variable
                    5
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
            derive_semigroup
            [t
            op]
            [(Require
              semigroup
              [t
              op]
            )]
          ),
      ... (semigroup requirement's tree)
      })
    semigroup_m
    [semigroup_m]
    .false.
    .false.
  )
```

## See Also

* [Symbols](symbol.md)

* [Requirement](Requirement.md)

* [Generics](../../generics.md)