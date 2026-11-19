! The explicit-shape result of a function is computed into the allocatable
! target it is assigned to, reallocated first to its shape (F2018
! 10.2.1.3), also when the target is a component, when the procedure of the
! assignment has internal procedures that the function cannot reach, and
! for logical and constant-length character elements. It is not when the
! function may see the target: the expression is evaluated before the
! target is defined.
module realloc_lhs_result_inplace_01_m
implicit none
type :: box_t
    real, allocatable :: v(:)
end type
type :: pair_t
    integer :: k = 5
    real, allocatable :: w(:)
end type
contains

pure function f3(a) result(r)
    real, intent(in) :: a(:)
    real :: r(size(a))
    r = 2*a + 1
end function

! The elements in reverse order: computed into an array the argument
! shares, it would read elements it has already written
pure function rev(a) result(r)
    real, intent(in) :: a(:)
    real :: r(size(a))
    integer :: i
    do i = 1, size(a)
        r(i) = a(size(a) + 1 - i)
    end do
end function

pure function g3(a) result(r)
    real, intent(in) :: a(:)
    logical :: r(size(a))
    r = a > 1
end function

pure function c3(a) result(r)
    real, intent(in) :: a(:)
    character(len=2) :: r(size(a))
    integer :: i
    do i = 1, size(a)
        r(i) = merge("hi", "lo", a(i) > 1)
    end do
end function

function pairs(n) result(r)
    integer, intent(in) :: n
    type(pair_t) :: r(n)
    integer :: i
    do i = 1, n
        if (allocated(r(i)%w)) error stop 40
        if (r(i)%k /= 5) error stop 41
        r(i)%w = [real(i)]
    end do
end function

! A procedure with internal procedures that f3 cannot reach
subroutine with_internal(x)
    real, intent(in) :: x(:)
    real, allocatable :: d(:)
    d = f3(x)
    call check(d)
    d = f3(x(2:3))
    call check2()
contains
    subroutine check(y)
        real, intent(in) :: y(:)
        if (any(y /= [3., 5., 7.])) error stop 20
    end subroutine
    subroutine check2()
        if (size(d) /= 2 .or. any(d /= [5., 7.])) error stop 21
    end subroutine
end subroutine

end module

program realloc_lhs_result_inplace_01
use realloc_lhs_result_inplace_01_m
implicit none
real, allocatable :: a(:), e(:)
real, allocatable, target :: t(:)
real, pointer :: p(:)
logical, allocatable :: m(:)
character(len=2), allocatable :: s(:)
type(box_t) :: b
type(pair_t), allocatable :: q(:)

a = [1., 2., 3.]

! A component: unallocated, another shape, the same shape with lower bound 0
b%v = f3(a)
if (size(b%v) /= 3 .or. lbound(b%v, 1) /= 1) error stop 1
if (any(b%v /= [3., 5., 7.])) error stop 2
b%v = f3(a(1:2))
if (size(b%v) /= 2 .or. any(b%v /= [3., 5.])) error stop 3
deallocate(b%v)
allocate(b%v(0:2))
b%v = f3(a)
if (lbound(b%v, 1) /= 0 .or. any(b%v /= [3., 5., 7.])) error stop 4
! A zero-size result
b%v = f3(a(1:0))
if (size(b%v) /= 0) error stop 5

! Logical and character elements
m = g3(a)
if (size(m) /= 3 .or. any(m .neqv. [.false., .true., .true.])) error stop 6
m = g3(a(1:1))
if (size(m) /= 1 .or. m(1)) error stop 7
s = c3(a)
if (size(s) /= 3 .or. any(s /= ["lo", "hi", "hi"])) error stop 8

! A derived type with default initialization and an allocatable component:
! computed into a temporary, the elements start initialized and unallocated
allocate(q(2))
q(1)%k = 9
q(1)%w = [7., 7.]
q = pairs(2)
if (q(1)%k /= 5 .or. size(q(1)%w) /= 1 .or. q(2)%w(1) /= 2) error stop 9

! A procedure with internal procedures that the function cannot reach
call with_internal(a)

! A target that a pointer reaches, read through it
t = [1., 2., 3.]
p => t
t = rev(p)
if (any(t /= [3., 2., 1.])) error stop 12

! An argument that reads the target
e = [1., 2.]
e = rev(e)
if (any(e /= [2., 1.])) error stop 13
print *, "ok"

end program
