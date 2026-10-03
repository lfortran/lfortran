program gpu_metal_377
! Test: do concurrent calling a submodule function whose implementation
! uses a module that has a module variable and a module procedure. With
! --separate-compilation the GPU offload pass loads that module to resolve
! the submodule's dependencies; it must only declare the module's symbols,
! so that they are not defined again next to the module's own object file.
use gpu_metal_377_m, only : compute
implicit none
integer :: i, y(4)

do concurrent (i = 1:4)
  y(i) = compute(i)
end do

print *, y
if (y(1) /= 11) error stop
if (y(2) /= 12) error stop
if (y(3) /= 13) error stop
if (y(4) /= 14) error stop
end program
