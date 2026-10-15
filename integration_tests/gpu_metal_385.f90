program gpu_metal_385
! Test: do concurrent calling a submodule function, compiled separately,
! that holds an `error stop` on a branch the data never takes. The offload
! pass only sees the body when it loads it for the kernel, after the
! unsupported-construct check, and lowers the stop there like any other.
use gpu_metal_385_m, only : scaled
implicit none
integer :: i
real :: a(4)

do concurrent (i = 1:4)
  a(i) = scaled(1, real(i))
end do

print *, a
if (any(abs(a - [2., 4., 6., 8.]) > 1e-6)) error stop
end program
