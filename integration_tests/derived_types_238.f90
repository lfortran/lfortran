! Whole array components as operands of elementwise operations, read in
! place when no pointer can reach them, and copied when they can overlap
! the assignment target (#16215)
module derived_types_238_m
implicit none

type :: inner
    real, allocatable :: x(:)
end type

type :: t
    real, allocatable :: x(:), y(:), b(:), z(:,:)
    real :: w(3)
    type(inner) :: in
end type

contains

subroutine add(a, c)
    type(t), intent(in) :: a
    real, intent(out) :: c(:)
    c = a%x + a%y
end subroutine

subroutine add_poly(a, c)
    class(t), intent(in) :: a
    real, intent(out) :: c(:)
    c = a%x * a%y
end subroutine

subroutine accumulate(self, dw)
    type(t), intent(inout) :: self
    real, intent(in) :: dw(:)
    self%b = self%b + dw
end subroutine

subroutine add_col(a, j)
    type(t), intent(inout) :: a
    integer, intent(in) :: j
    a%z(:,j) = a%x + a%y
end subroutine

subroutine neg(a, c)
    type(t), intent(in) :: a
    real, intent(out) :: c(:)
    c = -a%x
end subroutine

subroutine cmp(a, m)
    type(t), intent(in) :: a
    logical, intent(out) :: m(:)
    m = a%x > 2.0
end subroutine

subroutine nested(a, c)
    type(t), intent(in) :: a
    real, intent(out) :: c(:)
    c = a%in%x + 1
end subroutine

subroutine element(arr, i, c)
    type(t), intent(in) :: arr(:)
    integer, intent(in) :: i
    real, intent(out) :: c(:)
    c = arr(i)%x * arr(2)%y
end subroutine

subroutine fixed(a, c)
    type(t), intent(in) :: a
    real, intent(out) :: c(:)
    c = a%w + 1
end subroutine

subroutine shifted(a, n)
    type(t), intent(inout) :: a
    integer, intent(in) :: n
    a%x(2:n) = a%x(1:n-1) + a%y(1:n-1)
end subroutine

subroutine reversed(a, n)
    type(t), intent(inout) :: a
    integer, intent(in) :: n
    a%x(n:1:-1) = a%x + 1
end subroutine

subroutine where_mask(a, c)
    type(t), intent(inout) :: a
    real, intent(inout) :: c(:)
    where (a%x > 2.0)
        a%x = -a%x
        c = a%x
    end where
end subroutine

end module

! A function whose result is assigned to a component of a module variable
! that it reads in place as an operand (#16191).
module derived_types_238_g
implicit none

type :: tw
    real :: w(3)
end type

type(tw) :: g

contains

function f() result(rr)
    real :: rr(3)
    rr(3:1:-1) = g%w + 1
end function

function f1() result(rr)
    real :: rr(3)
    call fill1(rr)
end function

subroutine fill1(d)
    real, intent(out) :: d(3)
    d(3:1:-1) = g%w + 1
end subroutine

end module

program derived_types_238
use derived_types_238_m
use derived_types_238_g
implicit none
type(t) :: a
type(t) :: arr(2)
real :: c(3)
logical :: m(3)

allocate(a%x(3), a%y(3), a%b(3), a%z(3,2), a%in%x(3))
a%x = [1., 2., 3.]
a%y = [10., 20., 30.]
a%b = [1., 1., 1.]
a%z = 0
a%w = [4., 5., 6.]
a%in%x = [7., 8., 9.]

call add(a, c)
if (any(c /= [11., 22., 33.])) error stop 1
call add_poly(a, c)
if (any(c /= [10., 40., 90.])) error stop 2
call accumulate(a, [1., 2., 3.])
if (any(a%b /= [2., 3., 4.])) error stop 3
call add_col(a, 2)
if (any(a%z(:,2) /= [11., 22., 33.])) error stop 4
if (any(a%z(:,1) /= 0)) error stop 5
call neg(a, c)
if (any(c /= [-1., -2., -3.])) error stop 6
call cmp(a, m)
if (any(m .neqv. [.false., .false., .true.])) error stop 7
call nested(a, c)
if (any(c /= [8., 9., 10.])) error stop 8
call fixed(a, c)
if (any(c /= [5., 6., 7.])) error stop 9

allocate(arr(1)%x(3), arr(1)%y(3), arr(2)%x(3), arr(2)%y(3))
arr(1)%x = [1., 2., 3.]
arr(1)%y = [4., 5., 6.]
arr(2)%x = [2., 2., 2.]
arr(2)%y = [3., 3., 3.]
call element(arr, 1, c)
if (any(c /= [3., 6., 9.])) error stop 10
arr(2)%x = arr(1)%x + arr(2)%x
if (any(arr(2)%x /= [3., 4., 5.])) error stop 11

! The target overlaps an operand: the operand is copied first.
call shifted(a, 3)
if (any(a%x /= [1., 11., 22.])) error stop 12
a%x = [1., 2., 3.]
call reversed(a, 3)
if (any(a%x /= [4., 3., 2.])) error stop 13

! The mask is evaluated before the component is defined.
a%x = [1., 2., 3.]
c = 0
call where_mask(a, c)
if (any(a%x /= [1., 2., -3.])) error stop 14
if (any(c /= [0., 0., -3.])) error stop 15

! The operand g%w is read in place, so the result is evaluated before g%w
! is defined.
g%w = [1., 2., 3.]
g%w = f()
if (any(g%w /= [4., 3., 2.])) error stop 16
g%w = [1., 2., 3.]
g%w = f1()
if (any(g%w /= [4., 3., 2.])) error stop 17
print *, "ok"
end program
