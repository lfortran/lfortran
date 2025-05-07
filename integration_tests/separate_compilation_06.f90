program separate_compilation_06
use separate_compilation_06b_module

real :: R(4), Rp(4), rho(4)
real :: V(4)

R = 124.125
Rp = 0.125
rho = 0.1253512
V = rpoisson_outward_pc(R, Rp, rho)

end program

