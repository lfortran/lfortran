! A per-thread workspace of a `do concurrent` body whose extent calls a
! function without side effects: an array intrinsic of an argument
! (`minval(net%n)`, `maxval(net%n)`) or `mod`. Shared lowering turns each
! into a function of the translation unit that reads its arguments only, so
! the host calls it before the launch to size the workspace, as the device
! does for the array's extent.
module gpu_metal_389_m
implicit none
type :: net_t
    integer, allocatable :: n(:)
end type
contains
    subroutine run(net, x, m, o)
        type(net_t), intent(in) :: net
        real, intent(in) :: x(:)
        integer, intent(in) :: m
        real, intent(out) :: o(:)
        integer :: i
        do concurrent (i = 1:size(x))
            block
                real :: t(maxval(net%n)), u(mod(m, 3) + 2)
                real, allocatable :: w(:)
                allocate(w(minval(net%n) + 1))
                t = x(i)
                u = 1
                w = 2
                o(i) = sum(t) + sum(u) + sum(w)
            end block
        end do
    end subroutine
end module

program gpu_metal_389
use gpu_metal_389_m
implicit none
type(net_t) :: net
real :: x(4), o(4)
integer :: i
allocate(net%n(3))
net%n = [2, 5, 3]
x = [(real(i), i = 1, 4)]
call run(net, x, 7, o)
do i = 1, 4
    if (abs(o(i) - (5*i + 3 + 6)) > 1e-5) error stop 1
end do
print *, "ok"
end program
