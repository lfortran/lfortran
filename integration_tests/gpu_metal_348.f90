module gpu_metal_348_m
! A `do concurrent` over real(4) allocatable components of a polymorphic
! dummy whose type also holds deferred-length character components, one of
! its own and one in a nested type, that the loop never reads. Metal has a
! type for every value the kernel reaches, so the loop is offloaded there:
! the character components never reach the device and do not keep it off.
implicit none
type map_t
    character(len=:), allocatable :: layer
    real, allocatable :: slope(:)
end type
type net_t
    type(map_t) :: input_map
    character(len=:), allocatable :: name
    real, allocatable :: weights(:), biases(:)
end type
contains
subroutine evaluate(net, y)
    class(net_t), intent(in) :: net
    real, intent(out) :: y(:)
    integer :: i
    do concurrent (i = 1:size(y))
        y(i) = net%weights(i) * net%weights(i) + net%biases(i)
    end do
end subroutine
end module

program gpu_metal_348
use gpu_metal_348_m
implicit none
integer, parameter :: n = 16
type(net_t) :: net
real :: y(n)
integer :: i
allocate(net%weights(n), net%biases(n))
net%input_map%layer = "input"
net%name = "net"
do i = 1, n
    net%weights(i) = real(i) / 4.0
    net%biases(i) = 0.5 * i
end do
call evaluate(net, y)
do i = 1, n
    if (abs(y(i) - (net%weights(i)**2 + 0.5 * i)) > 1.0e-5 * y(i)) error stop
end do
if (net%input_map%layer /= "input") error stop
if (net%name /= "net") error stop
print *, sum(y)
end program
