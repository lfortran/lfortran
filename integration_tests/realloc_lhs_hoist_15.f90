! The reallocation of the target of an explicit-shape function result in a
! loop, whose extent is kept in a scalar the loop sets just before it,
! leaves the loop when the shape cannot change, also in a procedure with
! internal procedures that do not name the target. An internal procedure
! that changes the allocation of the target keeps it in the loop.
module realloc_lhs_hoist_15_m
implicit none
type :: box_t
    real, allocatable :: v(:)
end type
contains
pure function f3(a) result(r)
    real, intent(in) :: a(:)
    real :: r(size(a))
    r = 2*a + 1
end function

subroutine component(n, s)
    integer, intent(in) :: n
    real, intent(out) :: s
    type(box_t) :: b
    real :: a(n)
    integer :: i
    a = 1
    s = 0
    do i = 1, 10
        a(1) = real(i)
        b%v = f3(a)
        s = s + b%v(1)
    end do
    if (size(b%v) /= n .or. lbound(b%v, 1) /= 1) error stop 1
end subroutine

subroutine untouched(n, s)
    integer, intent(in) :: n
    real, intent(out) :: s
    real, allocatable :: c(:)
    real :: a(n)
    integer :: i, k
    a = 1
    s = 0
    k = 0
    do i = 1, 10
        a(1) = real(i)
        c = f3(a)
        call count()
        s = s + c(1)
    end do
    if (k /= 10 .or. size(c) /= n) error stop 2
contains
    subroutine count()
        k = k + 1
    end subroutine
end subroutine

subroutine reallocated(n, s)
    integer, intent(in) :: n
    real, intent(out) :: s
    real, allocatable :: c(:)
    real :: a(n)
    integer :: i
    a = 1
    s = 0
    do i = 1, 10
        a(1) = real(i)
        c = f3(a)
        s = s + c(1) + size(c)
        call shrink()
    end do
    if (size(c) /= 1) error stop 3
contains
    subroutine shrink()
        deallocate(c)
        allocate(c(0:0))
        c = 0
    end subroutine
end subroutine
end module

program realloc_lhs_hoist_15
use realloc_lhs_hoist_15_m
implicit none
real :: s
call component(3, s)
if (abs(s - 120) > 1e-4) error stop 10
call untouched(4, s)
if (abs(s - 120) > 1e-4) error stop 11
call reallocated(2, s)
if (abs(s - 140) > 1e-4) error stop 12
print *, "ok"
end program
