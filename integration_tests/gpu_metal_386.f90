! An array assignment whose value reads its own target only element by
! element, at the very same section, needs no temporary: `b(1:n(l),l) =
! b(1:n(l),l) - eta`. The two sections are spelled with a subscript read
! from another array, `n(l)`, and with an associate name for a component.
! They were not recognised as the same section, so the assignment was taken
! to need a run-time sized temporary and the loop was not offloaded.
module gpu_metal_386_m
implicit none

type :: net_t
    integer, allocatable :: n(:)
    real, allocatable :: w(:,:,:), b(:,:)
end type

contains

    subroutine adjust(net, eta, m)
        type(net_t), intent(inout) :: net
        real, intent(in) :: eta
        integer, intent(in) :: m
        integer :: l
        associate(w => net%w, b => net%b, n => net%n)
            do concurrent (l = 1:ubound(n,1))
                b(1:n(l),l) = b(1:n(l),l)/m
                b(1:n(l),l) = b(1:n(l),l) - eta*b(1:n(l),l)
                w(1:n(l),1:n(l-1),l) = w(1:n(l),1:n(l-1),l) - eta
            end do
        end associate
    end subroutine

    ! The same section inside a serial loop of the body.
    subroutine accumulate(n, d, r)
        integer, intent(in) :: n(0:)
        real, intent(in) :: d(:,:)
        real, intent(inout) :: r(:,:,:)
        integer :: k, l
        do concurrent (k = 1:size(r,3))
            do l = 1, ubound(n,1)
                r(1:n(l),l,k) = r(1:n(l),l,k) + d(1:n(l),l)*k
            end do
        end do
    end subroutine

end module

program gpu_metal_386
use gpu_metal_386_m
implicit none
type(net_t) :: net
real :: d(3,2), r(3,2,4)
integer :: k, l

allocate(net%n(0:2))
net%n = [3, 2, 3]
allocate(net%w(3,3,2), net%b(3,2))
net%w = 2.0
net%b = 4.0
call adjust(net, 0.5, 2)
do l = 1, 2
    if (any(abs(net%b(1:net%n(l),l) - 1.0) > 1.0e-6)) error stop 1
    if (any(abs(net%w(1:net%n(l),1:net%n(l-1),l) - 1.5) > 1.0e-6)) error stop 2
end do
if (abs(net%b(3,1) - 4.0) > 1.0e-6) error stop 3
if (any(abs(net%w(3,:,1) - 2.0) > 1.0e-6)) error stop 4

d = 1.0
r = 0.0
call accumulate(net%n, d, r)
do k = 1, 4
    if (any(abs(r(1:2,1,k) - k) > 1.0e-6)) error stop 5
    if (abs(r(3,1,k)) > 1.0e-6) error stop 6
    if (any(abs(r(1:3,2,k) - k) > 1.0e-6)) error stop 7
end do
print *, "ok"
end program
