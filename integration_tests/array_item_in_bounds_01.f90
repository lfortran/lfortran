! Element references whose subscripts are all constants within the constant
! bounds of the array are known to be in bounds and need no runtime check:
! `a(8, 3)`, `lb(-2)`, `s%u(4)`, `names(2)` and `b(5)` for the explicit-shape
! dummy `b(2:5)`. References with a variable subscript, or into an array
! whose bounds are not constants (allocatable, pointer, assumed-shape,
! assumed-size), and the array-valued `w%u(1)` are still checked.
module array_item_in_bounds_01_m
implicit none
real :: a(8, 3)
type :: t
    real :: u(4)
end type
contains
subroutine fill(b, c, d)
real, intent(out) :: b(2:5)
real, intent(inout) :: c(:)
real, intent(inout) :: d(*)
b = 0
b(2) = 1
b(5) = 4
c(1) = c(1) + 10
d(2) = d(2) + 20
end subroutine
end module

program array_item_in_bounds_01
use array_item_in_bounds_01_m, only: a, t, fill
implicit none
integer :: lb(-2:2), i
real :: b(4), c(2), d(2)
real, allocatable :: x(:)
real, pointer :: p(:)
real, target :: q(3)
type(t) :: s, w(2)
character(len=3) :: names(2)

a = 0
a(1, 1) = 1
a(8, 3) = 24
i = 2
a(i, 2) = 10
if (abs(sum(a) - 35) > 0) error stop
if (abs(a(8, 3) - 24) > 0) error stop
if (abs(a(1, 1) - 1) > 0) error stop

lb = 0
lb(-2) = 5
lb(2) = 7
if (lb(-2) /= 5 .or. lb(2) /= 7 .or. sum(lb) /= 12) error stop

s%u = 0
s%u(4) = 3
w(1)%u = 0
w(2)%u = 0
w%u(1) = 2
if (abs(s%u(4) - 3) > 0 .or. abs(sum(s%u) - 3) > 0) error stop
if (abs(sum(w(1)%u) + sum(w(2)%u) - 4) > 0) error stop

names(1) = "abc"
names(2) = "xyz"
if (names(2) /= "xyz" .or. names(1)(2:3) /= "bc") error stop

allocate(x(3))
x = 0
x(1) = 1
x(3) = 3
if (abs(sum(x) - 4) > 0) error stop

q = 0
p => q
p(2) = 5
if (abs(q(2) - 5) > 0) error stop

c = 1
d = 2
call fill(b, c, d)
if (abs(b(1) - 1) > 0 .or. abs(b(4) - 4) > 0 .or. abs(sum(b) - 5) > 0) error stop
if (abs(c(1) - 11) > 0 .or. abs(c(2) - 1) > 0) error stop
if (abs(d(1) - 2) > 0 .or. abs(d(2) - 22) > 0) error stop
print *, "ok"
end program
