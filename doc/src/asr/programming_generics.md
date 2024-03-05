# Programming with Generics in LFortran

In this page we will explore how to write generics in LFortran.

## Prerequisite

Variables inside of a generic function are typed with a *deferred types*. For example, we may want to define a function that calculate the sum of a generic array which can take any number type. Using a deferred type `T`, we may want to declare the following generic function:

```fortran
function array_sum(arr) result(r)
  type(T), intent(in) :: arr(:)
  type(T) :: r
  r = set_to_zero()
  do i = 1, n
    ! uses a function instead of + because T 
    ! can be any type, even a derived type
    r = add_element(arr(i), r)
  end do
end function
```

As we can see here, we need to define the functions associated with the deferred type `T`. To do so in LFortran, we use *requirements* to define both deferred types and their associated functions.

```fortran
requirement number_type(T, add_element, set_to_zero)
  type, deferred :: T
  function add_element(x, y) result(z)
    type(T), intent(in) :: x, y
    type(T) :: z
  end function
  function set_to_zero() result(z)
    type(T) :: z
  end function
end requirement
```

## Defining and Using a Generic Function

Having defined the necessary requirement to use the deferred type `T`, we can proceed to define our generic array sum. LFortran provides two different ways to write and instantiate generic functions:

1. With a template
2. Without a template (simpler syntax)

### With Template

**Defining a generic function**

A template works as a closure for a generic function. A template takes as parameters symbols that would replace the generic symbols inside a generic function.

```fortran
template array_t(T, add_element, set_to_zero)
  ...
  public :: array_sum
contains
  function array_sum(arr) result(r)
    type(T), intent(in) :: arr(:)
    type(T) :: r
    integer :: n, i
    n = size(arr)
    r = set_to_zero(0)
    if (n > 0) then
      r = arr(1)
      do i = 2, n
        res = add_element(r, arr(i))
      end do
    end if
  end function
end template
```

Then to connect the parameters with the functions defined in the requirement, we use a `require` statement.

```fortran
template array_t(T, add_element, set_to_zero)
  require :: number_type(T, add_element, set_to_zero)
  public :: array_sum
contains
  function array_sum(arr) result(r)
    type(T), intent(in) :: arr(:)
    type(T) :: r
    integer :: n, i
    n = size(arr)
    r = set_to_zero(0)
    if (n > 0) then
      r = arr(1)
      do i = 2, n
        res = add_element(r, arr(i))
      end do
    end if
  end function
end template
```

`require :: number_type(T, add_element, set_to_zero)` sets the type signature for the parameters within the scope of the template. This makes it possible for the LFortran compiler to type check the computations associated with the deferred type `T`.

A template can also contains multiple functions that may depend on each other.

```fortran
template array_t(T, add_element, set_to_zero)
  require :: number_type(T, add_element, set_to_zero)
  public :: array_sum
contains
  function array_sum(arr) result(r)
    type(T), intent(in) :: arr(:)
    type(T) :: r
    integer :: n, i
    n = size(arr)
    r = set_to_zero(0)
    if (n > 0) then
      r = arr(1)
      do i = 2, n
        res = add_element(r, arr(i))
      end do
    end if
  end function
  
  function array_avg(arr) result(r)
    type(T), intent(in) :: arr(:)
    type(T) :: s
    s = array_sum(arr)
    ...
  end function
end template
```

**Using a generic function**

To use a generic function we first need to instantiate (replace) the generic symbols inside a template with symbols with concrete types. The instantiation is done through the `instantiate` statement. For example, if we want to instantiate 'array_sum' with integer types, the instantiation would be as follows:

```fortran
instantiate array_t(integer, add_element_integer, set_to_zero_integer), &
  only: array_sum_integer => array_sum
```

First we pass the concrete symbols to the template in the form of a template call `array_t(integer, add_element_integer, set_to_zero_integer)`. We are replacing the deferred type `T` with a concrete type `integer`, `add_element` with a function that computes addition between two integers `add_element_integer`, and `set_to_zero` with a function that returns a zero integer value `set_to_zero_integer`. These functions would have to be defined prior to the instantiation for them to be passed as symbol arguments to a template:

```fortran
function add_element_integer(x, y) result(z)
  integer, intent(in) :: x, y
  integer :: z
  z = x + y
end function

function set_to_zero_integer() result(z)
  integer :: z
  z = 0
end function
```

After `only: ` we decide which function inside in the template we want to instantiate, in this case `array_sum`, and give the new instantiated function a new name `array_sum_integer`.

After instantiation, we can then use the instantiated function similar to ordinary functions:

```fortran
integer :: arr(10), sum
sum = array_sum_integer(arr)
```

The main benefit of generics is reuse. We can have different instantiations for different types using the same template. For example we can instantiate both integer and real `array_sum`:

```fortran
! instantiation with integer type
instantiate array_t(integer, add_element_integer, set_to_zero_integer), &
  only: array_sum_integer => array_sum

! instantiation with real type
instantiate array_t(real, add_element_real, set_to_zero_real), &
  only: array_sum_real => array_sum
```

Also, because a template may contain multiple generic functions, a single instantiation can be used to instantiate multiple functions:

```fortran
instantiate array_t(integer, add_element_integer, set_to_zero_integer), &
  only: array_sum_integer => array_sum, array_avg_integer => array_avg

instantiate array_t(real, add_element_real, set_to_zero_real), &
  only: array_sum_real => array_sum, array_avg_real => array_avg
```

### Without Template

The template notation can be cumbersome for defining a single generic function. To alleviate this, LFortran also supports a simpler syntax for declaring generic functions without having to declare an enclosing template. For example, our running `array_sum` example can be written as follows in the simpler syntax:

```fortran
function generic_sum {T, add_element, set_to_zero} (arr) result(r)
  require :: number_type(T, add_element, set_to_zero)
  type(T), intent(in) :: arr(:)
  type(T) :: r
  integer :: n, i
  n = size(arr)
  r = set_to_zero(0)
  if (n > 0) then
    r = arr(1)
    do i = 2, n
      res = add_element(r, arr(i))
    end do
  end if
end function
```

The first difference is that the template parameters are now included as the function's generic symbol parameters enclosed by braces `{T, add_element, set_to_zero}`. Since we still need to identify these generic symbols a requirement, the require statement is moved into the generic function itself. The rest of the function is the same as the generic function inside the template.

This is merely a syntax sugar for the original templated function. Inside the compiler this generic function is treated as the following template:

```fortran
template generic_sum(T, add_element, set_to_zero)
  require :: number_type(T, add_element, set_to_zero)
  public :: generic_sum
contains
  function generic_sum(arr) result(r)
    type(T), intent(in) :: arr(:)
    type(T) :: r
    integer :: n, i
    n = size(arr)
    r = set_to_zero(0)
    if (n > 0) then
      r = arr(1)
      do i = 2, n
        res = add_element(r, arr(i))
      end do
    end if
  end function
end template
```

The instantiation is also made simpler by having the instantiation and function call together:

```fortran
integer :: arr(10), sum
sum = array_sum{integer, add_element_integer, set_to_zero_integer}(arr)
```

### Further Simplifying Instantiations

**Passing operator instead of functions**

So far to replace the generic addition `add_element` we have used a concrete function `add_element_integer`. To simplify this, it is possible to just pass `operator(+)` without having to define a function separately:

```fortran
instantiate array_t(integer, operator(+), set_to_zero_integer), &
  only: array_sum_integer => array_sum
```

**Skipping function instantiation names**

Generic functions can also be instantiated without having to rename each function one-by-one. Suppose we want to instantiate every generic functions inside the template `array_t`, we can shorten the instantiation into:

```fortran
instantiate array_t(integer, operator(+), set_to_zero_integer)
```

Doing so would generate the function `array_sum` and `array_avg` without any renaming.

## Generic Derived Types

LFortran also supports generic derived types. Let's say we want a generic tuple. We can define a derived type for tuples as usual inside a template:

```fortran
template derived_type_t(T)
    ! for brevity we have the deferred type
    ! declared directly inside the template
    type, deferred :: T
    public :: tuple

    type :: tuple
      type(T) :: fst
      type(T) :: snd
    end type
end template
```

We can also define generic functions accessing this generic tuple as:

```fortran
template derived_type_t(T)
    type, deferred :: T
    public :: tuple

    type :: tuple
      type(T) :: fst
      type(T) :: snd
    end type
contains
    function get_fst(p) result(r)
      type(tuple), intent(in) :: p
      type(t) :: r
      r = p%fst
    end function

    function get_snd(p) result(r)
      type(tuple), intent(in) :: p
      type(t) :: r
      r = p%snd
    end function
end template
```

The instantiation for derived types are also similar to generic functions. If we want an integer tuple type and its functions, then we can instantiate `derived_type_t` as:

```fortran
instantiate derived_type_t(integer), only: &
  tuple_int => tuple, get_fst_int => get_fst, get_snd_int => get_snd
```

## See Also

* [Generics](generics.md), for details about the generics implementation and its related ASR.