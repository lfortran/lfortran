! Positional type parameter values of a parameterized derived type follow
! the order of its type parameter list, not the order of the declarations
! of the parameters
module pdt_24_mod
implicit none

type :: t(a, b)
    integer, kind :: b = 4
    integer, kind :: a = 8
    real(a) :: x
    real(b) :: y
end type

type, extends(t) :: u(c, d)
    integer, kind :: d
    integer, kind :: c
    integer(c) :: i
    integer(d) :: j
end type

end module

program pdt_24
use pdt_24_mod, only: t, u
implicit none

type :: p(m, n)
    integer, kind :: n, m
    integer :: arr(m, n)
end type

type(t(4, 8)) :: v
type(t(4)) :: w
type(u(4, 8, 2, 8)) :: z
type(p(2, 3)) :: q

if (v%a /= 4 .or. v%b /= 8) error stop 1
if (kind(v%x) /= 4 .or. kind(v%y) /= 8) error stop 2
if (w%a /= 4 .or. w%b /= 4) error stop 3
if (kind(w%x) /= 4 .or. kind(w%y) /= 4) error stop 4
if (z%a /= 4 .or. z%b /= 8 .or. z%c /= 2 .or. z%d /= 8) error stop 5
if (kind(z%i) /= 2 .or. kind(z%j) /= 8) error stop 6
if (q%m /= 2 .or. q%n /= 3) error stop 7
if (any(shape(q%arr) /= [2, 3])) error stop 8
print *, v%a, v%b, w%a, w%b, z%a, z%b, z%c, z%d, shape(q%arr)
end program
