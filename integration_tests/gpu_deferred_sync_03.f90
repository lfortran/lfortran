! A module subroutine that launches may return with its launches pending,
! through the twin its callers in this file call; they wait before they
! access what the launches write. Each case below must still see the
! values the kernels wrote: on the host, through a module variable, after a
! kernel that can stop, through a procedure pointer, a type-bound call, a
! non-contiguous section, recursion, and a chain of such calls.
module gpu_deferred_sync_03_mod
implicit none
real :: shared(64)
type :: box
contains
    procedure, nopass :: run => add_one
end type
contains
subroutine add_one(x)
    real, intent(inout) :: x(:)
    integer :: i
    do concurrent (i = 1:size(x))
        x(i) = x(i) + 1
    end do
end subroutine

subroutine scale(x, s)
    real, intent(inout) :: x(:)
    real, intent(in) :: s
    integer :: i
    do concurrent (i = 1:size(x))
        x(i) = s*x(i)
    end do
end subroutine

subroutine copy_into(x, y)
    real, intent(in) :: x(:)
    real, intent(out) :: y(:)
    integer :: i
    real :: tmp(size(x))
    do concurrent (i = 1:size(x))
        tmp(i) = x(i) + 1
    end do
    do concurrent (i = 1:size(x))
        y(i) = 2*tmp(i)
    end do
end subroutine

subroutine to_shared(x)
    real, intent(in) :: x(:)
    integer :: i
    do concurrent (i = 1:size(shared))
        shared(i) = x(i) + 3
    end do
end subroutine

subroutine checked(x, n)
    real, intent(inout) :: x(:)
    integer, intent(in) :: n
    integer :: i
    do concurrent (i = 1:size(x))
        if (n < 0) error stop "negative"
        x(i) = x(i) + n
    end do
end subroutine

subroutine explicit(x, n)
    integer, intent(in) :: n
    real, intent(inout) :: x(n)
    integer :: i
    do concurrent (i = 1:n)
        x(i) = x(i) + 10
    end do
end subroutine

recursive subroutine repeat(x, k)
    real, intent(inout) :: x(:)
    integer, intent(in) :: k
    if (k == 0) return
    call add_one(x)
    call repeat(x, k - 1)
end subroutine

subroutine chain(x, y)
    real, intent(inout) :: x(:)
    real, intent(out) :: y(:)
    call add_one(x)
    call scale(x, 2.0)
    call copy_into(x, y)
end subroutine
end module

program gpu_deferred_sync_03
use gpu_deferred_sync_03_mod
implicit none
real :: x(64), y(64), m(8, 8)
procedure(add_one), pointer :: p
type(box) :: b
integer :: i
x = 1
call chain(x, y)
! x = (1 + 1)*2 = 4, y = 2*(4 + 1) = 10
if (any(abs(x - 4) > 0)) error stop 1
if (any(abs(y - 10) > 0)) error stop 2
call to_shared(x)
if (any(abs(shared - 7) > 0)) error stop 3
call checked(x, 5)
if (any(abs(x - 9) > 0)) error stop 4
p => add_one
call p(x)
if (any(abs(x - 10) > 0)) error stop 5
call b%run(x)
if (any(abs(x - 11) > 0)) error stop 6
call repeat(x, 3)
if (any(abs(x - 14) > 0)) error stop 7
do i = 1, 8
    m(i, :) = i
end do
call explicit(m(2, :), 8)
if (any(abs(m(2, :) - 12) > 0)) error stop 8
call add_one(m(:, 3))
if (abs(m(2, 3) - 13) > 0) error stop 9
if (abs(m(5, 3) - 6) > 0) error stop 10
call scale(y, sum(x(1:2)))
if (any(abs(y - 280) > 0)) error stop 11
print *, x(1), y(1), shared(1), m(2, 3)
end program
