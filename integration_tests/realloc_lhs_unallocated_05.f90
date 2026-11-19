! An `allocated(x)` test of an array that is certainly unallocated is false:
! only the ELSE branch runs. This is also the test before the move of a
! function result into its target: a first assignment of an allocatable
! result moves it without a test.
module realloc_lhs_unallocated_05_m
implicit none
contains
pure function numbers(n) result(r)
    integer, intent(in) :: n
    real, allocatable :: r(:)
    integer :: i
    r = [(real(i), i = 1, n)]
end function

subroutine first(n, s)
    integer, intent(in) :: n
    real, intent(out) :: s
    real, allocatable :: x(:), t(:)
    x = numbers(n)
    if (size(x) /= n .or. lbound(x, 1) /= 1) error stop 1
    if (allocated(t)) error stop 2
    if (.not. allocated(t)) then
        allocate(t(0:n))
    else
        error stop 3
    end if
    t = 1
    s = sum(x) + sum(t)
end subroutine

subroutine maybe(flag, n)
    logical, intent(in) :: flag
    integer, intent(in) :: n
    real, allocatable :: t(:)
    if (flag) allocate(t(2))
    if (allocated(t)) then
        if (.not. flag) error stop 4
    else
        if (flag) error stop 5
    end if
    t = numbers(n)
    if (size(t) /= n) error stop 6
end subroutine

subroutine fresh(o, n)
    real, allocatable, intent(out) :: o(:)
    integer, intent(in) :: n
    if (allocated(o)) error stop 7
    o = numbers(n)
end subroutine
end module

program realloc_lhs_unallocated_05
use realloc_lhs_unallocated_05_m
implicit none
real :: s
real, allocatable :: o(:)
call first(3, s)
if (abs(s - 10) > 1e-6) error stop 8
call maybe(.true., 3)
call maybe(.false., 2)
allocate(o(5))
call fresh(o, 2)
if (size(o) /= 2 .or. o(2) /= 2) error stop 9
print *, "ok"
end program
