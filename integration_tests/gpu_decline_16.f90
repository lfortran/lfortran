! gpu decline reason: HeapArrayUse
! The result of `pick` is allocated on the device heap, because the caller
! cannot work out its shape, and handed to `pick` as an allocatable dummy
! argument, which the procedure would have to allocate itself.
module gpu_decline_16_mod
implicit none
contains
pure function pick(c) result(r)
    real, intent(in) :: c(:)
    real, allocatable :: r(:)
    r = merge(c, -c, c > 2)
end function
end module

program gpu_decline_16
use gpu_decline_16_mod
implicit none
real :: x(3, 2), s(2)
integer :: k
x = reshape([1., 2., 3., 4., 5., 6.], [3, 2])
do concurrent (k = 1:2)
    s(k) = sum(pick(x(:, k)))
end do
print *, s
if (any(s /= [0., 15.])) error stop
end program
