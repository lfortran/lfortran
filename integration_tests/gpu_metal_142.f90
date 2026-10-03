program gpu_metal_142
! Test: bind(C) enum in a submodule type-bound procedure called from
! do concurrent with --gpu=metal --separate-compilation.  Verifies the
! gpu_offload pass resolves ExternalSymbols for nameless enum modules
! when loading submodule (.smod) files.
use gpu_metal_142_net_m, only: net_t
implicit none
type(net_t) :: net
real :: x(4), y(4)

x = [1.0, 2.0, 3.0, 4.0]
y = 0.0

call net%run(x, y, 4)

print *, y
if (abs(y(1) - 2.0) > 1e-6) error stop
if (abs(y(2) - 4.0) > 1e-6) error stop
if (abs(y(3) - 6.0) > 1e-6) error stop
if (abs(y(4) - 8.0) > 1e-6) error stop
end program
