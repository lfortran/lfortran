module separate_compilation_06b_module

use separate_compilation_06a_module
contains

function rpoisson_outward_pc(R, Rp, rho) result(V)
real, intent(in) :: R(:), Rp(:), rho(:)
real :: V(size(R))

integer :: N
integer, parameter :: max_it = 2
real :: rho_mid(3), pi = 3.14159265358979323846

N = size(R)
rho_mid = get_midpoints(R(:4), rho(:4))
print *, "sum(rho_mid): ", sum(rho_mid)
if ( abs(sum(rho_mid) - 3.76053631e-01 ) > 1e-8 ) error stop
end function

end module


