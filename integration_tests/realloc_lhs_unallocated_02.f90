! Assignments whose target may be allocated where they execute, which keep
! the test of its allocation and shape (allocate_unallocated): the target
! was assigned before, on some path or in an earlier iteration of a loop,
! by an internal procedure, passed to a procedure, moved into, named through
! an associate name, is saved, or optional. An ALLOCATE of an allocated
! array would stop this program without --fast, and a missing reallocation
! would give the wrong size.
module realloc_lhs_unallocated_02_m
implicit none
type :: tensor_t
    real, allocatable :: values_(:)
end type
contains

pure function assigned_twice(n) result(r)
    integer, intent(in) :: n
    integer, allocatable :: r(:)
    r = [n]
    r = [n, n, n]
end function

pure function assigned_on_some_path(n) result(r)
    integer, intent(in) :: n
    integer, allocatable :: r(:)
    if (n > 0) r = [n]
    r = [n, n]
end function

pure function in_a_loop(n) result(r)
    integer, intent(in) :: n
    integer, allocatable :: r(:)
    integer :: i, j
    do i = 1, n
        r = [(j, j = 1, i)]
    end do
end function

subroutine allocate_it(x)
    integer, allocatable, intent(inout) :: x(:)
    allocate(x(5))
    x = 7
end subroutine

function passed_down(n) result(r)
    integer, intent(in) :: n
    integer, allocatable :: r(:)
    call allocate_it(r)
    r = [n, n]
end function

function moved_into(n) result(r)
    integer, intent(in) :: n
    integer, allocatable :: r(:), t(:)
    allocate(t(4))
    t = n
    call move_alloc(t, r)
    r = [n]
end function

function by_internal_procedure(n) result(r)
    integer, intent(in) :: n
    integer, allocatable :: r(:)
    call set()
    r = [n, n, n]
contains
    subroutine set()
        r = [1]
    end subroutine
end function

function through_associate(n) result(t)
    integer, intent(in) :: n
    type(tensor_t) :: t
    associate(q => t)
        q%values_ = [real(n)]
    end associate
    t%values_ = [real(n), real(n)]
end function

subroutine saved(n, total)
    integer, intent(in) :: n
    integer, intent(inout) :: total
    integer, allocatable, save :: w(:)
    integer :: i
    w = [(n, i = 1, n)]
    total = total + sum(w)
end subroutine

subroutine optional_out(x, n)
    integer, allocatable, intent(out), optional :: x(:)
    integer, intent(in) :: n
    if (present(x)) x = [n, n]
end subroutine

end module

program realloc_lhs_unallocated_02
use realloc_lhs_unallocated_02_m
implicit none
type(tensor_t) :: t
integer, allocatable :: x(:)
integer :: total

if (size(assigned_twice(4)) /= 3) error stop 1
if (size(assigned_on_some_path(4)) /= 2) error stop 2
if (size(in_a_loop(4)) /= 4) error stop 3
if (any(passed_down(5) /= [5, 5])) error stop 4
if (any(moved_into(6) /= [6])) error stop 5
if (any(by_internal_procedure(2) /= [2, 2, 2])) error stop 6
t = through_associate(3)
if (any(t%values_ /= [3.0, 3.0])) error stop 7
total = 0
call saved(2, total)
call saved(3, total)
if (total /= 4 + 9) error stop 8
x = [1, 2, 3]
call optional_out(x, 4)
if (any(x /= [4, 4])) error stop 9
call optional_out(n=4)
print *, "ok"
end program
