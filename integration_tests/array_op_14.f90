module array_op_14_mod
implicit none
type :: t
    integer :: x = 0
end type
type :: u
    integer :: y = 0
end type
interface assignment(=)
    module procedure assign_u
end interface
integer :: ncalls = 0
contains
elemental subroutine assign_u(a, b)
    type(u), intent(out) :: a
    integer, intent(in) :: b
    a%y = 10 * b
end subroutine
function f(i) result(r)
    integer, intent(in) :: i
    type(t) :: r
    ncalls = ncalls + 1
    r%x = i + ncalls
end function
function g(i) result(r)
    integer, intent(in) :: i
    integer :: r
    ncalls = ncalls + 1
    r = i + ncalls
end function
function h(i) result(r)
    integer, intent(in) :: i
    real :: r
    ncalls = ncalls + 1
    r = i + ncalls
end function
function s(i) result(r)
    integer, intent(in) :: i
    character(len=3) :: r
    ncalls = ncalls + 1
    write(r, '(i3)') i + ncalls
end function
elemental function e(a, b) result(r)
    integer, intent(in) :: a, b
    integer :: r
    r = a + b
end function
subroutine fill(a)
    integer, intent(out) :: a(:)
    a = g(4)
end subroutine
end module

program array_op_14
! A scalar expression assigned to an array, or used as an operand of an
! array expression, is evaluated once, not once per element.
use array_op_14_mod
implicit none
type(t) :: at(3), at2(2, 2)
type(u) :: au(3)
integer :: ai(3), bi(3), ai2(2, 3), k
real :: ar(4)
character(len=3) :: ac(3)

at = f(4)
print *, "ncalls:", ncalls, " at%x:", at%x
if (ncalls /= 1) error stop
if (any(at%x /= 5)) error stop

ncalls = 0
at%x = 0
at(2:3) = f(4)
if (ncalls /= 1) error stop
if (any(at%x /= [0, 5, 5])) error stop

ncalls = 0
at2 = f(1)
if (ncalls /= 1) error stop
if (any(at2%x /= 2)) error stop

ncalls = 0
ai = g(4)
if (ncalls /= 1) error stop
if (any(ai /= 5)) error stop

ncalls = 0
ai = 0
ai(1:2) = g(4)
if (ncalls /= 1) error stop
if (any(ai /= [5, 5, 0])) error stop

ncalls = 0
ai2 = g(1)
if (ncalls /= 1) error stop
if (any(ai2 /= 2)) error stop

ncalls = 0
ar = h(2)
if (ncalls /= 1) error stop
if (any(abs(ar - 3.0) > 1.0e-6)) error stop

ncalls = 0
ac = s(1)
if (ncalls /= 1) error stop
if (any(ac /= "  2")) error stop

ncalls = 0
at%x = g(4)
if (ncalls /= 1) error stop
if (any(at%x /= 5)) error stop

bi = [1, 2, 3]
ncalls = 0
ai = g(4) + bi
if (ncalls /= 1) error stop
if (any(ai /= [6, 7, 8])) error stop

ncalls = 0
ai = bi * g(4) - 1
if (ncalls /= 1) error stop
if (any(ai /= [4, 9, 14])) error stop

ncalls = 0
ai = g(4) + g(4)
if (ncalls /= 2) error stop
if (any(ai /= 11)) error stop

ncalls = 0
ai = e(bi, g(4) + 1)
if (ncalls /= 1) error stop
if (any(ai /= [7, 8, 9])) error stop

ncalls = 0
ai = 0
where (g(1) > bi) ai = 1
if (ncalls /= 1) error stop
if (any(ai /= [1, 0, 0])) error stop

ncalls = 0
au = g(4)
if (ncalls /= 1) error stop
if (any(au%y /= 50)) error stop

ncalls = 0
do k = 1, 2
    ai = g(k)
end do
if (ncalls /= 2) error stop
if (any(ai /= 4)) error stop

ncalls = 0
call fill(ai)
if (ncalls /= 1) error stop
if (any(ai /= 5)) error stop
end program
