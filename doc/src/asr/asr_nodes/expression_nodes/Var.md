# Var

**Var** is an expression node that represents a reference to a declared
variable, function, enum, etc.

## Declaration

### Syntax

```fortran
Var(symbol v)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|   `v`   | symbol name |

### Return values

The return value is the expression that the Var represents.

## Description

`Var` represents a declaration when used within an expression. Every
declaration is represented in the symbol table, for example:

* `integer :: x`: here `x` is represented by `Variable`, which has `type` as an
  "Integer" type, `type_declaration` is empty
* `procedure(fn) :: x`: here `x` is represented by `Variable`, which has `type`
  as a "FunctionType" type, and `type_declaration` as a "Function" symbol
  representing the interface declaration "fn"
* `function my_fn(...) ... end function`: represented by `Function`

When these are to be used in an expression, one uses `Var` which points to one
of these.

For example, if we have a declaration `integer :: x`, then the assignment `x =
5` is represented as `(Assignment (Var 2 x) (IntegerConstant 5 (Integer 4 []))
())`. Here `Var` points to the `Variable` symbol in the symbol table. See below
for a full example.

Another example: if `procedure(fn), pointer :: f`, then `f => myf` is
represented by `(Associate (Var 2 f) (Var 2 myf))`, where `myf` is a
`Function`.

Most often `Var` points to a `Variable`. For referencing functions it points to
`Function`. For enums it points to `Enum`. These can be hidden behind
`ExternalSymbol`. It cannot point to any other symbol.

## Types

`Var`'s argument `v` can point to the following symbols (and nothing else):
* `Variable` (for expressions like `2*x+5` where `x` is a variable)
* `Function` (for expressions like `myf`, where `myf` is a user defined
  function, such as passing it as a callback argument to a function call `call
  f(myf)`, or assigning to a pointer procedure variable `f => myf`)
* `Enum`
* `ExternalSymbol` (the above symbols can be behind an `ExternalSymbol` if they
  are declared in another module)

## Examples

```fortran
program test_var
integer :: x
x = 5
print *, 3*x+5
end program
```

ASR:

```
(TranslationUnit
    (SymbolTable
        1
        {
            test_var:
                (Program
                    (SymbolTable
                        2
                        {
                            x:
                                (Variable
                                    2
                                    x
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
                    test_var
                    []
                    [(=
                        (Var 2 x)
                        (IntegerConstant 5 (Integer 4 []))
                        ()
                    )
                    (Print
                        ()
                        [(IntegerBinOp
                            (IntegerBinOp
                                (IntegerConstant 3 (Integer 4 []))
                                Mul
                                (Var 2 x)
                                (Integer 4 [])
                                ()
                            )
                            Add
                            (IntegerConstant 5 (Integer 4 []))
                            (Integer 4 [])
                            ()
                        )]
                        ()
                        ()
                    )]
                )
        })
    []
)
```

## See Also

