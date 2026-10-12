! Length type parameters with constant values: a component of a
! parameterized derived type whose length type parameter is a length type
! parameter of the enclosing type or a constant, a negative length type
! parameter, and a structure constructor with type parameter values
program pdt_27
implicit none

type :: vec(n)
    integer, len :: n
    integer :: a(n)
end type

type :: outer(m)
    integer, len :: m
    type(vec(m)) :: inner
    type(vec(1024)) :: big
end type

type :: named(k, l)
    integer, kind :: k = 4
    integer, len :: l = 3
    character(len=l) :: name
    real(k) :: v(l)
end type

type(outer(6)) :: w
type(outer(1000)) :: v
type(vec(-3)) :: q
type(named(8, 5)) :: e

w%inner%a = 3
w%big%a = 2
v%inner%a = 1
if (w%m /= 6 .or. w%inner%n /= 6 .or. size(w%inner%a) /= 6) error stop 1
if (sum(w%inner%a) /= 18) error stop 2
if (w%big%n /= 1024 .or. size(w%big%a) /= 1024) error stop 3
if (sum(w%big%a) /= 2048) error stop 4
if (v%m /= 1000 .or. v%inner%n /= 1000 .or. sum(v%inner%a) /= 1000) error stop 5
if (v%big%n /= 1024) error stop 6

if (q%n /= -3 .or. size(q%a) /= 0) error stop 7

e = named(8, 5)("hello world", 2.5_8)
if (e%name /= "hello" .or. size(e%v) /= 5 .or. any(e%v /= 2.5_8)) error stop 8
e = named(l=5, k=8)("abc", 1.5_8)
if (e%name /= "abc  " .or. any(e%v /= 1.5_8)) error stop 9
print *, w%m, w%inner%n, w%big%n, v%m, v%inner%n, v%big%n, q%n, e%name
end program
