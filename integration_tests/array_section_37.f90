module array_section_37_mod
implicit none

type :: holder
    integer :: v(3)
    real :: w(2, 2)
end type

contains

subroutine fill(d, e)
    real, intent(out) :: d(:)
    integer, intent(inout) :: e(0:3)
    d = [1.5, 2.5, 3.5]
    d(2:3) = [-1.0, -2.0]
    e(1:3) = [7, 8, 9]
end subroutine

end module

program array_section_37
use array_section_37_mod, only: holder, fill
implicit none
real :: a(4, 6), r(3), s(0:2)
real(8) :: a8(2, 3)
integer :: k(5), m(3, 3), e(0:3), i
logical :: l(2, 2), mask(4)
complex :: z(2)
character(len=2) :: c(3)
integer, allocatable :: x(:)
real, target :: t(6)
real, pointer :: p(:)
type(holder) :: h

a = 0.0
a(:, 1) = [1.0, 2.0, 3.0, 4.0]
a(:, 2) = [5.0, 6.0, 7.0, 8.0]
a(:, 3) = [1.0, 2.0, 3.0, 4.0]
a(2:3, 4) = [-1.0, -2.0]
a(1:4:2, 5) = [10.0, 30.0]
a(3, 2:6:2) = [0.5, 0.25, 0.125]
if (any(a(:, 1) /= [1.0, 2.0, 3.0, 4.0])) error stop 1
if (any(a(:, 2) /= [5.0, 6.0, 0.5, 8.0])) error stop 2
if (any(a(:, 3) /= [1.0, 2.0, 3.0, 4.0])) error stop 3
if (any(a(:, 4) /= [0.0, -1.0, 0.25, 0.0])) error stop 4
if (any(a(:, 5) /= [10.0, 0.0, 30.0, 0.0])) error stop 5
if (any(a(:, 6) /= [0.0, 0.0, 0.125, 0.0])) error stop 6

! The right-hand side references the left-hand side
a(:, 1) = [a(2, 1), a(1, 1), a(4, 1), a(3, 1)]
if (any(a(:, 1) /= [2.0, 1.0, 4.0, 3.0])) error stop 7

! Repeated in a loop
do i = 1, 6
    a(:, i) = [real(i), 2.0, 3.0, 4.0]
    a(2:4, i) = [20.0, 30.0, 40.0]
end do
do i = 1, 6
    if (any(a(:, i) /= [real(i), 20.0, 30.0, 40.0])) error stop 8
end do

! Implied-do array constructor
k = [(i * i, i = 1, 5)]
if (any(k /= [1, 4, 9, 16, 25])) error stop 9
k(2:4) = [(-i, i = 1, 3)]
if (any(k /= [1, -1, -2, -3, 25])) error stop 10

! Vector subscript on the left-hand side
k([5, 1]) = [50, 10]
if (k(1) /= 10 .or. k(5) /= 50) error stop 11

! Lower bound other than one
s = [7.0, 8.0, 9.0]
if (s(0) /= 7.0 .or. s(2) /= 9.0) error stop 12

! Other types and kinds
a8(:, 2) = [1.0d0, 2.0d0]
a8(1, :) = [3.0_8, 4.0_8, 5.0_8]
if (any(a8(:, 2) /= [4.0d0, 2.0d0])) error stop 13
if (a8(1, 3) /= 5.0d0) error stop 14
a8(:, 1) = [1.0, 2.0]
if (a8(2, 1) /= 2.0d0) error stop 15
m = 0
m(2, :) = [1, 2, 3]
m(:, 3) = [4, 5, 6]
if (any(m(2, :) /= [1, 2, 5])) error stop 16
l(:, 1) = [.true., .false.]
l(:, 2) = [.false., .true.]
if (.not. l(1, 1) .or. l(2, 1) .or. l(1, 2) .or. .not. l(2, 2)) error stop 17
z = [(1.0, 2.0), (3.0, -4.0)]
if (z(2) /= (3.0, -4.0)) error stop 18
c = ['ab', 'cd', 'ef']
c(2:3) = ['xy', 'zw']
if (c(1) /= 'ab' .or. c(3) /= 'zw') error stop 19

! Allocatable left-hand side
allocate(x(3))
x = [1, 2, 3]
if (size(x) /= 3 .or. x(3) /= 3) error stop 20
x(2:3) = [20, 30]
if (any(x /= [1, 20, 30])) error stop 21

! Pointer left-hand side
t = 0.0
p => t(2:6:2)
p = [1.0, 2.0, 3.0]
if (any(t /= [0.0, 1.0, 0.0, 2.0, 0.0, 3.0])) error stop 22

! Derived type components
h%v = [4, 5, 6]
h%w(:, 2) = [1.0, 2.0]
if (h%v(2) /= 5 .or. h%w(2, 2) /= 2.0) error stop 23

! Masked assignment and block construct
mask = [.true., .false., .true., .false.]
where (mask) a(:, 1) = [-1.0, -2.0, -3.0, -4.0]
if (any(a(:, 1) /= [-1.0, 20.0, -3.0, 40.0])) error stop 24
block
    real :: b(3)
    b = [3.0, 2.0, 1.0]
    r = b
end block
if (any(r /= [3.0, 2.0, 1.0])) error stop 25

! Dummy arguments
e = 0
call fill(r, e)
if (any(r /= [1.5, -1.0, -2.0])) error stop 26
if (any(e /= [0, 7, 8, 9])) error stop 27

print *, "ok"
end program
