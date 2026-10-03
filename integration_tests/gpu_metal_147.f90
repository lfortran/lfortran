program gpu_metal_147
! Test: gpu_offload resolves submodule type-bound procedure symbol
! from an external module during do concurrent offloading with
! --separate-compilation. Previously the transitive function
! collector did not load submodules from disk, so the nested
! type-bound procedure call inside the submodule body was missed.
use gpu_metal_147_neural_network, only : neural_network_t
implicit none
type(neural_network_t) :: nn
real :: x(10), y(10)
integer :: i

x = 1.0
y = 0.0

do concurrent (i = 1:10)
  y(i) = nn%infer(x(i))
end do

print *, y
if (abs(y(1) - 1.0) > 1e-6) error stop
if (abs(y(5) - 1.0) > 1e-6) error stop
if (abs(y(10) - 1.0) > 1e-6) error stop
end program
