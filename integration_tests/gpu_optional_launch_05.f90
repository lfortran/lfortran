! An allocatable actual of a CONTIGUOUS optional dummy, a component through
! an associate name or a whole variable, allocated or not, is passed without
! checking or copying it on the host: the launches run without host waits.
module gpu_optional_launch_05_m
implicit none
type :: layer
    real, allocatable :: w(:)
end type
contains
pure subroutine scale(x, y, w)
    real, contiguous, intent(in) :: x(:)
    real, contiguous, intent(out) :: y(:)
    real, contiguous, optional, intent(in) :: w(:)
    integer :: i
    if (present(w)) then
        do concurrent (i = 1:size(x))
            y(i) = x(i) * w(i)
        end do
    else
        do concurrent (i = 1:size(x))
            y(i) = x(i) + 1.0
        end do
    end if
end subroutine

subroutine forward(layers, x, y)
    type(layer), intent(in) :: layers(:)
    real, contiguous, intent(inout) :: x(:), y(:)
    integer :: l
    do l = 1, size(layers)
        associate (lw => layers(l))
            call scale(x, y, lw%w)
            call scale(y, x, lw%w)
        end associate
    end do
end subroutine

subroutine forward_whole(w, x, y)
    real, allocatable, intent(in) :: w(:)
    real, contiguous, intent(inout) :: x(:), y(:)
    call scale(x, y, w)
    call scale(y, x, w)
end subroutine
end module

program gpu_optional_launch_05
use gpu_optional_launch_05_m
implicit none
integer, parameter :: n = 1000, nl = 6
type(layer) :: layers(nl)
real, allocatable :: x(:), y(:), w(:)
integer :: l
allocate(x(n), y(n))
do l = 1, nl
    allocate(layers(l)%w(n))
    layers(l)%w = 1.0
end do
layers(2)%w = 2.0
layers(2)%w(n) = 0.5
x = 1.5
! Layer 2 multiplies by 4 (by 0.25 at n), the others by 1.
call forward(layers, x, y)
if (any(abs(x(:n-1) - 6.0) > 1e-6)) error stop 1
if (abs(x(n) - 0.375) > 1e-6) error stop 2
if (any(abs(y(:n-1) - 6.0) > 1e-6)) error stop 3
! Not allocated, w is absent in `scale`, which adds 1 twice.
call forward_whole(w, x, y)
if (any(abs(x(:n-1) - 8.0) > 1e-6)) error stop 4
allocate(w(n))
w = 3.0
call forward_whole(w, x, y)
if (any(abs(x(:n-1) - 72.0) > 1e-5)) error stop 5
if (abs(x(n) - 21.375) > 1e-5) error stop 6
print *, x(1), x(n)
end program
