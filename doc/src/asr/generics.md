# Generics

From a high-level perspective generics are supported in LFortran by three main elements:

1. **Requirements** declaring deferred types and their abstract functions

2. **Templates** implementing generic symbols (functions, subroutines, derived types) using deferred types and abstract functions described by requirements

3. **Instantiations** by passing concrete types and functions into templates

## Requirements

Requirements declare deferred types (generic types) and its associated functions, similar to *typeclasses* in Haskell and *traits* in Rust. For example, the signature for a generic monoid of any type can be represented by the following requirement:

```fortran
requirement monoid(T, op)
  ! declaring a deferred type (generic type)
  type, deferred :: T
  ! declaring a function associated with the deferred type
  function op(x, y) result(z)
    type(T), intent(in) :: x, y
    type(T) :: z
  end function
  function empty() result(z)
    type(T) :: z
  end function
end requirement
```

The `monoid` requirement declares a deferred type `T` and two functions,`op` that takes arguments of type `T` and return a value with the same type `T` and `empty` that returns a value of type `T`. The deferred type `T` is internally interpreted as a variable `T` typed as `TypeParameter T` (for brevity we will write it just as `T`). Functions declared in requirements are abstract (without function body). 

On ASR level, the requirement is represented by the symbol [`Requirement`](asr_nodes/symbol_nodes/Requirement.md). ASR for requirements are built during symbol table visit in the function `visit_Requirement`. `monoid`'s symbol table would contain a variable `T` with the type `T`, a function `op` with the type `T * T -> T`, and a function `empty` with the type `() -> T`. 

A requirement is checked to see if all the parameters have a corresponding symbol declared inside of it. A warning is generated if a parameter is declared but no symbol is found with the same name.

On its own, a requirement does not do any computation. It is also not compiled into the target language when the ASR is compiled.

## Templates

Templates take the role of a scope for generic functions. A template uses generic types and its associated type signatures obtained from requirements to check generic operations. 

As a running example, we will consider a generic function for n-times multiplication for argument of any type. This function can be represented as:

```fortran
module generics_example
  ! same requirement as before
  requirement monoid(T, op, empty)
    ...
  end requirement

  ! the template starts from here
  template array_t(S, op_temp, empty_temp)
    require :: monoid(S, op_temp, empty_temp)
  contains
    ! below is the generic function
    function array_sum(arr) result(r)
      type(S), intent(in) :: arr(:)
      type(S) :: r
      integer :: n, i
      n = size(arr)
      r = empty_temp(0)
      if (n > 0) then
        r = arr(1)
        do i = 2, n
          res = op_temp(r, arr(i))
        end do
      end if
    end function
  end template
end module
```

The template `array_t` contains the generic function `array_sum` that takes an array `arr` of type `S`. The template later on will be added to the parent symbol table as a `Template` symbol with the name `array_t`.

Typing context has to be obtained to type the parameter `arr` and the addition operation `op_temp(r, arr(i))`. Such typing context can be made available within the scope of the template by using a requirement. Here, the `Require` statement `require :: monoid(S, op_temp, empty_temp)` builds the types of the template's parameters `S`, `op_temp`, and `empty_temp` based on the types of the symbols in `monoid`.

This corresponds to the `visit_UnitRequire` that can be found in `visit_Template` during symbol table visit. Calling a require statement copies the symbols from the requiremement and replaces their names with the argument names given by the require statement. In this case, the symbol `T`, `op`, and `empty` from the requirement `monoid` are replaced by `S`, `op_temp`, and `empty_temp` that are passed as arguments. This replacement is done by the function `rename_symbol`.

Eventually the require statement adds into `array_t`'s symbol table two symbols,  `S` as a variable with type `S`, `op_temp` and `empty_temp` as functions with types `S * S -> S` and `() -> S` based on `monoid`'s symbol table.

Symbol table visit checks the variable declarations in `array_sum`. Since `S` is available in the symbol table now, both variables `arr` and `r` can be typed. Later during body visit, with type `S`, `op_temp`, and `empty_temp` in the symbol table, the function call `op_temp(r, arr(i))` in `array_sum` would be checked in the same way as non-generic functions. 

ASR representation of templates are also not compiled into the target language.

## Instantiations

Generic functions need to be instantiated with concrete types and functions to be used in run-time. The process of instantiation replaces the generic types in the function definition with concrete types (such as `integer`, `real`) and replace the abstract functions with implemented functions. 

The generic function described in the previous section can be instantiated as a function that computes array sum by the following instantiation:

```fortran
module functions
  public :: add_integer, empty_integer
contains
  function add_integer(x, y)
    integer, intent(in) :: x, y
    integer :: add_integer
    add_integer = x + y
  end function
  function empty_integer()
    integer :: empty_integer
    empty_integer = 0
  end function
end module

program instantiate_template
use functions, only: add_integer, empty_integer

instantiate array_t(integer, add_integer, empty_integer), &
  only: array_sum_integer => array_sum

! function argument for basic arithmetic operations can be replaced with an operator
instantiate array_t(integer, operator(+), empty_integer), &
  only: array_sum_integer => array_sum

! eliding function renaming would instantiating all generic functions with the same name
instantiate array_t(integer, operator(+), empty_integer)
end program
```

This instantiation statement
```
instantiate array_t(integer, add_integer, empty_integer), only: array_sum_integer => array_sum
```
passes the type `integer`, an integer addition function `add_integer`, and a function describing empty integer value `empty_integer` as arguments to template `array_t`, then instantiates the function `array_sum` as a new function named `array_sum_integer`. This instantiation wants to replace `S` in `array_t` with the type `integer`, `op_temp` function calls with `add_integer` function calls, and `empty_temp` function calls with `empty_integer` function calls.

### Type Checking

Before a function is generated on ASR level by an instantiation, the compiler checks the consistency of its type substitution based on the given symbol arguments. Currently there is no notion of subtyping in LFortran, so checking is limited to exact type checks. This is done by tracking the type substitutions made by the symbol arguments and rejecting any contradicting type subsitutition. Checking is done during symbol table visit in `visit_Instantiate`.

We will explain this in detail through the instantiation example above. The first argument `integer` substitutes the type parameter `S` in template `array_t` (maps `S` to `integer`). The second argument `add_integer` substitutes the type `S * S -> S` of `op_temp` with `integer * integer -> integer` (checks `S` substitution). This is allowed because the function argument gives a consistent substitution with the previous argument. If it passes the type checks, the instantiated function would be generated as a function on ASR level. Successfully checked symbol arguments are put inside the variable `type_subs` for type substitution (`S` to `integer`) and `symbol_subs` for function substitution (`op_temp` to `add_integer`).

Now, suppose that we have the following instantiation where instead of `integer`, we pass `real` as an argument: 
```
instantiate array_t(real, add_integer, empty_integer), only: array_sum_integer => array_sum
```
Here, the first type argument maps `S` to `real` but the second function argument maps `S` to `integer`, resulting in a contradiction. Hence ending in a type error.

This check is tracked by the two variables `type_subs` and `symbol_subs`. Function arguments are checked by the function `check_restriction` during symbol table visit.

**Handling Operator Arguments**

Instantiations can be made simpler for basic binary operations (addition, multiplication, etc.) by passing `operator(<sign>)` instead of pre-defined functions.

If the type substitution tracked by `type_subs` is consistent for a binary arithmetic operation, then a function is implicitly generated as a function argument. From the example before, we will have a function that is equivalent to the following LFortran function generated implicitly:

```fortran
function ~add_intrinsic(arg0, arg1) result(ret)
  integer, intent(in) :: arg0, arg1
  integer :: ret
  ret = arg0 + arg1
end function
```

### ASR Generation

Instantiating the function as a function on the ASR level is simply a symbol replacement process.

The generation process is split into two, generating the function signature during symbol table visit and generating the function body during body visit. The whole generation is not done during body visit because derived types generated from a template may be used to type variable declarations.

During symbol table visit, after checking arguments and placing the symbol substitution into `type_subs` and `symbol_subs`, the instantiated function is added into the symbol table and its signature is built by the function `instantiate_symbol` where the deferred types are replaced with concrete types. For the example above, a function equivalent to the following is generated:

```fortran
elemental function array_sum_integer(arr) result(res)
  integer, intent(in) :: arr(:)   ! type replaced
  integer :: r                    ! type replaced
  integer :: n, i
end function
```

The substitution in `type_subs` and `symbol_subs` are preserved and then passed to the body visitor. During body visit, the body of the instantiated function is built in `visit_Instantiate` by the function `instantiate_body`, replacing any abstract functions with concrete functions:

```fortran
function array_sum_integer(n, a) result(res)
  integer, intent(in) :: arr(:)
  integer :: r
  integer :: n, i
  n = size(arr)
  r = empty_integer(0)              ! function call replaced
  if (n > 0) then
    r = arr(1)
    do i = 2, n
      res = add_integer(r, arr(i))  ! function call replaced
    end do
  end if
  end do
end function
```

## See Also

* [Programming With Generics](programming_generics.md), for simpler explaining about using generics in LFortran