# Generics

From a high-level perspective generics are supported in LFotran by three main elements:

1. **Requirements** declaring generic types and their functions

2. **Templates** implementing generic functions using generic types and functions described by requirements

3. **Instantiations** by passing concrete types and functions into templates

## Requirements

Requirements declare generic types and its associated functions, similar to *typeclasses* in Haskell and *traits* in Rust. For example, the signature for a generic semigroup of any types can be represented by the following requirement:

```fortran
requirement semigroup(T, op)
  type, deferred :: T
  function op(x, y) result(z)
    type(T), intent(in) :: x, y
    type(T) :: z
  end function
end requirement
```

The `semigroup` requirement declares a generic type `T` and a function `op` that takes arguments of type `T` and return a value with the same type. Functions declared in requirements are abstract (without implementation). 

ASR for requirements are built during symbol table visit. On ASR level, the requirement is represented by the symbol [`Requirement`](asr_nodes/symbol_nodes/Requirement.md). `semigroup`'s symbol table would contain a variable `T` with the type `T` and a function `op` with the type `T * T -> T`. 

On its own, a requirement does not do any computation. It is also not compiled into the target language when the ASR is compiled.

## Templates

Templates define generic functions using type signatures obtained from requirements. For example, a generic function for n-times multiplication can be represented by the following template:

```fortran
module semigroup_m
  requirement semigroup(T, op)
    ...
  end requirement

  template derive_semigroup(S, temp_op)
    require :: semigroup(S, temp_op)
  contains
    elemental function stimes(n, a) result(res)
      integer, intent(in) :: n
      type(S), intent(in) :: a
      type(S) :: res
      integer :: i
      res = a
      do i = 2, n
        res = temp_op(res, a)
      end do
    end function
  end template
end semigroup
```

The template `derive_semigroup` contains the generic function `stimes` that takes an integer `n` and a generic `a`.

Typing context has to be obtained to type the parameter `a` and the multiplication operation `temp_op(res, a)`. Such typing context can be made available within the scope of the template by using a requirement. Here, the `Require` statement `require :: semigroup(S, temp_op)` builds the types of the template's parameters `S` and `temp_op` based on the types of the symbols in `semigroup`. 

Symbol visit adds a `derive_semigroup` as a `Template` symbol to the parent symbol table. The visit checks `Require` statements and adds into `derive_semigroup`'s symbol table two symbols, namely `S` as a variable with type `S` and `temp_op` as a function with type `S * S -> S` based on `semigroup`'s symbol table. 

<!-- Add a type mapping -->

Symbol visit checks the variable declarations in `stimes`. Since `S` is available in the symbol table now, both variables `a` and `res` can be typed. Later during body visit, with type `S` and `temp_op` in the symbol table, the function call `temp_op(res, a)` in `stimes` would be checked in the same way as non-generic functions. 

ASR representation of templates are also not compiled into the target language.

## Instantiations

Generic functions need to be instantiated with concrete types and functions to be used in run-time. The process of instantiation replaces the generic types in the function definition with concrete types (such as `integer`, `real`) and replace the abstract functions with implemented functions. 

The generic function described in the previous section can be instantiated as a function that computes integer multiplications by the following instantiation:

```fortran
module functions
  public :: integer_mult
contains
  function integer_mult(x, y)
    integer, intent(in) :: x, y
    integer :: integer_mult
    integer_mult = x * y
  end function
end module

program instantiate_template
use functions, only: integer_mult

instantiate derive_semigroup(integer, integer_mult), only: integer_stimes => stimes
! can also be written as below for simplicity
instantiate derive_semigroup(integer, operator(*)), only: integer_stimes_op => stimes
end program
```

This instantiation statement
```
instantiate derive_semigroup(integer, integer_mult), only: integer_stimes => stimes
```
passes the type `integer` and an integer multiplication function `integer_mult` as arguments to template `derive_semigroup`, then instantiates the function `stimes` as a new function named `integer_stimes`. This instantiation wants to replace `S` in `derive_semigroup` with the type `integer` and `temp_op` function calls with `integer_mult` function calls.

 Instantiations can be made simpler or basic binary operations (addition, multiplication, etc.) by passing `operator(<sign>)` instead of functions.

### Type Checking

Before a function is generated on ASR level by an instantiation, the compiler checks the consistency of its type substitution based on the given symbol arguments. Currently there is no notion of subtyping in LFortran, so checking is limited to exact type checks. This is done by tracking the type substitutions made by the symbol arguments and rejecting any contradicting type subsitutition.

We will explain this through the instantiation example above. The first argument `integer` substitutes the type parameter `S` in template `derive_semigroup` (maps `S` to `integer`). The second argument `integer_mult` substitutes the type `S * S -> S` of `temp_op` with `integer * integer -> integer` (checks `S` substitution). This is allowed because the function argument gives a consistent substitution with the previous argument. If it passes the type checks, the instantiated function would be generated as a function on ASR level.

Now, suppose that we have the following instantiation where instead of `integer`, we pass `real` as an argument. 
```
instantiate derive_semigroup(real, integer_mult), only: integer_stimes_wrong => stimes
```

The first type argument maps `S` to `real` but the second function argument maps `S` to `integer`, resulting in contradiction. Hence ending in a type error.

### ASR Generation

Instantiating the function as a function on the ASR level is simply a symbol replacement process. 

The instantiation statement above would generate on the ASR level a function that is equivalent to the following Fortran function:
```fortran
elemental function integer_stimes(n, a) result(res)
  integer, intent(in) :: n
  integer, intent(in) :: a      ! type replaced
  integer :: res                ! type replaced
  integer :: i
  res = a
  do i = 2, n
    res = integer_mult(res, a)  ! function call replaced
  end do
end function
```