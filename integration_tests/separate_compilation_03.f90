program separate_compilation_03
use separate_compilation_03a_module
use separate_compilation_03b_module

real :: x0(5)
real :: res1, res2
x0 = 9124.124

res1 = mixing_anderson(x0)
print *, mixing_anderson(x0)
res2 = integrate_trapz_1(x0)
print *, integrate_trapz_1(x0)

if (abs(res1 - res2) > 1e-8) error stop
end program
