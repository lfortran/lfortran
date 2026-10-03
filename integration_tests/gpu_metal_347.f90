module gpu_metal_347_m
! A `do concurrent` reading a real(8) allocatable component of a
! polymorphic dummy whose type also holds a character component, ahead of
! the real(8) one, in a nested parameterized type. Metal has no 64-bit
! float, so the loop has to run on the CPU with a warning naming real(8):
! the character component is not what keeps it off the device, and naming
! it turned the loop into a compile error. CUDA has `double`, so there the
! loop is offloaded.
implicit none
integer, parameter :: dp = kind(1.0d0)
type map_t(k)
    integer, kind :: k = kind(1.0)
    character(len=:), allocatable :: layer
    real(k), allocatable :: slope(:)
end type
type net_t(k)
    integer, kind :: k = kind(1.0)
    type(map_t(k)) :: input_map
    real(k), allocatable :: weights(:), biases(:)
end type
contains
subroutine evaluate(net, y)
    class(net_t(dp)), intent(in) :: net
    real(dp), intent(out) :: y(:)
    integer :: i
    do concurrent (i = 1:size(y))
        y(i) = net%weights(i) * net%weights(i) + net%biases(i)
    end do
end subroutine
end module

program gpu_metal_347
use gpu_metal_347_m
implicit none
integer, parameter :: n = 16
type(net_t(dp)) :: net
real(dp) :: y(n)
integer :: i
allocate(net%weights(n), net%biases(n))
net%input_map%layer = "input"
do i = 1, n
    net%weights(i) = real(i, dp) / 3.0_dp
    net%biases(i) = 1.0e-12_dp * i
end do
call evaluate(net, y)
do i = 1, n
    if (abs(y(i) - (net%weights(i)**2 + 1.0e-12_dp * i)) > 1.0e-13_dp * y(i)) &
        error stop
end do
if (net%input_map%layer /= "input") error stop
print *, sum(y)
end program
