# Generics

From a high-level perspective generics are supported in LFortran by three main elements:

1. **Requirements** declaring deferred types and their abstract functions

2. **Templates** implementing generic symbols (functions, subroutines, derived types) using deferred types and abstract functions described by requirements

3. **Instantiations** by passing concrete types and functions into templates

## Requirements

Requirements declare deferred types (generic types) and its associated functions, similar to *typeclasses* in Haskell and *traits* in Rust. For example, the signature for a generic semigroup of any type can be represented by the following requirement:

```fortran
requirement semigroup(T, op)
  ! declaring a generic type parameter
  type, deferred :: T
  ! declaring a function associated with generic types
  function op(x, y) result(z)
    type(T), intent(in) :: x, y
    type(T) :: z
  end function
end requirement
```

The `semigroup` requirement declares a generic type `T` and a function `op` that takes arguments of type `T` and return a value with the same type. The deferred type `T` is internally interpreted as a variable `T` typed as `TypeParameter T`. Functions declared in requirements are abstract (without function body). 

On ASR level, the requirement is represented by the symbol [`Requirement`](asr_nodes/symbol_nodes/Requirement.md). ASR for requirements are built during symbol table visit in the function `visit_Requirement`. `semigroup`'s symbol table would contain a variable `T` with the type `T` and a function `op` with the type `T * T -> T`. 

A requirement is checked to see if all the parameters have a corresponding symbol declared inside of it. A warning is generated if a parameter is declared but no symbol is found with the same name.

On its own, a requirement does not do any computation. It is also not compiled into the target language when the ASR is compiled.

## Templates

Templates take the role of a scope for generic functions. A template uses generic types and its associated type signatures obtained from requirements to check generic operations. 

As a running example, we will consider a generic function for n-times multiplication. This function can be represented as:

```fortran
module semigroup_m
  requirement semigroup(T, op)
    ...
  end requirement

  ! the template starts from here
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

The template `derive_semigroup` contains the generic function `stimes` that takes an integer `n` and a generic typed `a`. Symbol table visit eventually will add `derive_semigroup` as a `Template` symbol to the parent symbol table.

Typing context has to be obtained to type the parameter `a` and the multiplication operation `temp_op(res, a)`. Such typing context can be made available within the scope of the template by using a requirement. Here, the `Require` statement `require :: semigroup(S, temp_op)` builds the types of the template's parameters `S` and `temp_op` based on the types of the symbols in `semigroup`.

This corresponds to the `visit_UnitRequire` that can be found in `visit_Template` during symbol table visit. Calling a require statement copies the symbols from the requiremement and replaces their names with the argument names given by the statement. In this case, the symbol `T` and `op`from the requirement `semigroup` are replaced by `S` and `temp_op` passed as arguments. This replacement is done by the function `rename_symbol`.

Eventually the require statement adds into `derive_semigroup`'s symbol table two symbols,  `S` as a variable with type `S` and `temp_op` as a function with type `S * S -> S` based on `semigroup`'s symbol table.

Symbol table visit checks the variable declarations in `stimes`. Since `S` is available in the symbol table now, both variables `a` and `res` can be typed. Later during body visit, with type `S` and `temp_op` in the symbol table, the function call `temp_op(res, a)` in `stimes` would be checked in the same way as non-generic functions. 

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

! function argument for basic arithmetic operations can be replaced with an operator
instantiate derive_semigroup(integer, operator(*)), only: integer_stimes_op => stimes

! eliding function renaming would instantiating all generic functions with the same name
instantiate derive_semigroup(integer, operator(*))
end program
```

This instantiation statement
```
instantiate derive_semigroup(integer, integer_mult), only: integer_stimes => stimes
```
passes the type `integer` and an integer multiplication function `integer_mult` as arguments to template `derive_semigroup`, then instantiates the function `stimes` as a new function named `integer_stimes`. This instantiation wants to replace `S` in `derive_semigroup` with the type `integer` and `temp_op` function calls with `integer_mult` function calls.

### Type Checking

Before a function is generated on ASR level by an instantiation, the compiler checks the consistency of its type substitution based on the given symbol arguments. Currently there is no notion of subtyping in LFortran, so checking is limited to exact type checks. This is done by tracking the type substitutions made by the symbol arguments and rejecting any contradicting type subsitutition. Checking is done during symbol table visit in `visit_Instantiate`.

We will explain this in detail through the instantiation example above. The first argument `integer` substitutes the type parameter `S` in template `derive_semigroup` (maps `S` to `integer`). The second argument `integer_mult` substitutes the type `S * S -> S` of `temp_op` with `integer * integer -> integer` (checks `S` substitution). This is allowed because the function argument gives a consistent substitution with the previous argument. If it passes the type checks, the instantiated function would be generated as a function on ASR level. Successfully checked symbol arguments are put inside the variable `type_subs` for type substitution (`S` to `integer`) and `symbol_subs` for function substitution (`op` to `temp_op`).

Now, suppose that we have the following instantiation where instead of `integer`, we pass `real` as an argument: 
```
instantiate derive_semigroup(real, integer_mult), only: integer_stimes_wrong => stimes
```
Here, the first type argument maps `S` to `real` but the second function argument maps `S` to `integer`, resulting in contradiction. Hence ending in a type error.

This check is tracked by the two variables `type_subs` and `symbol_subs`. Checking function arguments are done by the function `check_restriction`.

**Handling Operator Arguments**

Instantiations can be made simpler for basic binary operations (addition, multiplication, etc.) by passing `operator(<sign>)` instead of pre-defined functions.

If the type substitution tracked by `type_subs` is consistent for a binary arithmetic operation, then a function is implicitly generated as a function argument. From the example before, we will have a function that is equivalent to the following LFortran function generated implicitly:

```fortran
function ~mul_intrinsic(arg0, arg1) result(ret)
  integer, intent(in) :: arg0, arg1
  integer :: ret
  ret = arg0 * arg1
end function
```

### ASR Generation

Instantiating the function as a function on the ASR level is simply a symbol replacement process.

The generation process is split into two, generating the function signature during symbol table visit and generating the function body during body visit. The whole generation is not done during body visit because derived types generated from a template may be used to type variable declarations.

During symbol table visit, after checking arguments and placing the symbol substitution into `type_subs` and `symbol_subs`, the instantiated function is added into the symbol table and its signature is built by the function `instantiate_symbol`. For the example above, a function equivalent to the following is generated:

```fortran
elemental function integer_stimes(n, a) result(res)
  integer, intent(in) :: n, a
  integer :: res
  integer :: i
end function
```

The substitution in `type_subs` and `symbol_subs` are preserved and then passed to the body visitor. During body visit, the body of the instantiated function is built in `visit_Instantiate` by the function `instantiate_body`:

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