program gpu_metal_135
! Test: submodule function returning a derived type called inside
! do concurrent with --gpu=metal and --separate-compilation.
! Verifies that the GPU offload pass resolves ExternalSymbols in
! the duplicated submodule function body.
use gpu_metal_135_m, only : my_t, make_t
implicit none
type(my_t) :: arr(5)
integer :: i
do concurrent(i = 1:5)
  arr(i) = make_t(i)
end do
print *, arr(1)%val, arr(3)%val, arr(5)%val
if (arr(1)%val /= 1) error stop
if (arr(2)%val /= 2) error stop
if (arr(3)%val /= 3) error stop
if (arr(4)%val /= 4) error stop
if (arr(5)%val /= 5) error stop
end program
