! A kind type parameter used in the bounds of an array component
module pdt_20_mod
implicit none

type :: w(k)
    integer, kind :: k = 3
    integer :: c(k)
end type

type :: g(k, m)
    integer, kind :: k = 4
    integer, kind :: m = 1
    integer(k) :: a(k)
    integer :: b(k, k+1)
    real :: r(0:k)
    integer :: s(-m:m, 2*k)
    integer :: e(m+1:k)
    integer :: x(max(k, 6))
end type

contains

subroutine set_default(x)
    type(w), intent(inout) :: x
    if (size(x%c) /= 3) error stop 1
    x%c = [7, 8, 9]
end subroutine

subroutine set_four(x)
    type(w(4)), intent(inout) :: x
    if (size(x%c) /= 4) error stop 2
    x%c = 5
end subroutine

end module

program pdt_20
use pdt_20_mod
implicit none

type :: p(n)
    integer, kind :: n
    integer :: v(n)
end type

type(w) :: s
type(w(3)) :: t
type(w(4)) :: u
type(w) :: arr(2)
type(w(4)) :: arr4(2)
type(g) :: g1
type(g(8, 3)) :: g2
type(p(5)) :: q

if (size(s%c) /= 3) error stop 3
s%c = [1, 2, 3]
if (any(s%c /= [1, 2, 3])) error stop 4

t%c = [4, 5, 6]
if (size(t%c) /= 3) error stop 5
if (any(t%c /= [4, 5, 6])) error stop 6

u%c = [1, 2, 3, 4]
if (size(u%c) /= 4) error stop 7
if (sum(u%c) /= 10) error stop 8

if (size(arr(1)%c) /= 3 .or. size(arr4(2)%c) /= 4) error stop 9
arr(1)%c = [1, 2, 3]
arr4(2)%c = [1, 2, 3, 4]
if (sum(arr(1)%c) /= 6 .or. sum(arr4(2)%c) /= 10) error stop 10

call set_default(s)
call set_four(u)
if (any(s%c /= [7, 8, 9])) error stop 11
if (any(u%c /= 5)) error stop 12

if (kind(g1%a) /= 4 .or. size(g1%a) /= 4) error stop 13
if (any(shape(g1%b) /= [4, 5])) error stop 14
if (size(g1%r) /= 5 .or. lbound(g1%r, 1) /= 0) error stop 15
if (any(shape(g1%s) /= [3, 8]) .or. lbound(g1%s, 1) /= -1) error stop 16
if (size(g1%e) /= 3 .or. lbound(g1%e, 1) /= 2) error stop 17
if (size(g1%x) /= 6) error stop 25

if (kind(g2%a) /= 8 .or. size(g2%a) /= 8) error stop 18
if (any(shape(g2%b) /= [8, 9])) error stop 19
if (size(g2%r) /= 9) error stop 20
if (any(shape(g2%s) /= [7, 16])) error stop 21
if (size(g2%e) /= 5 .or. lbound(g2%e, 1) /= 4) error stop 22
if (size(g2%x) /= 8) error stop 26

g1%b = 1
g1%a = 2
if (sum(g1%b) /= 20 .or. sum(g1%a) /= 8) error stop 23

q%v = [1, 2, 3, 4, 5]
if (size(q%v) /= 5 .or. sum(q%v) /= 15) error stop 24

print *, size(s%c), size(u%c), shape(g1%b), shape(g2%s), size(q%v)
end program
