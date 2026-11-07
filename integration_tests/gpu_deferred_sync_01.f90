! A call of a procedure whose body is known accesses what that body
! accesses: the host still waits for a launch before a call that reads what
! the launch wrote, through an argument, a module variable or a variable of
! the host procedure, and not before one that only asks for sizes.
module gpu_deferred_sync_01_mod
implicit none
real :: total(4)
contains
pure subroutine check(x, y)
    real, intent(in) :: x(:), y(:)
    if (size(x) /= size(y)) error stop "size mismatch"
end subroutine

pure real function first(x)
    real, intent(in) :: x(:)
    first = x(1)
end function

real function first_total()
    first_total = total(1)
end function

subroutine run(x, y, s)
    real, intent(inout) :: x(:), y(:)
    real, intent(out) :: s(4)
    integer :: i
    real :: host(4)
    do concurrent (i = 1:size(x))
        y(i) = x(i) + 1
    end do
    call check(x, y)
    s(1) = first(y)
    do concurrent (i = 1:4)
        total(i) = 10*i
    end do
    s(2) = first_total()
    do concurrent (i = 1:4)
        host(i) = 100*i
    end do
    s(3) = first_host()
    do concurrent (i = 1:size(x))
        x(i) = 2*y(i)
    end do
    call check(x, y)
    s(4) = first(x)
contains
    real function first_host()
        first_host = host(1)
    end function
end subroutine
end module

program gpu_deferred_sync_01
use gpu_deferred_sync_01_mod
implicit none
real :: x(100), y(100), s(4)
integer :: i
do i = 1, 100
    x(i) = i
end do
y = 0
call run(x, y, s)
if (abs(s(1) - 2) > 0) error stop 1
if (abs(s(2) - 10) > 0) error stop 2
if (abs(s(3) - 100) > 0) error stop 3
if (abs(s(4) - 4) > 0) error stop 4
if (abs(x(100) - 202) > 0) error stop 5
if (abs(y(100) - 101) > 0) error stop 6
if (abs(total(4) - 40) > 0) error stop 7
print *, s
end program
