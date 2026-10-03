program gpu_metal_144
! Test: do concurrent calling a submodule function whose implementation
! uses a separate module (transitive dependency). With --gpu=metal
! --separate-compilation, the GPU offload pass must load the transitive
! module dependency when resolving the submodule's ExternalSymbol.
use gpu_metal_144_m, only : compute
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
