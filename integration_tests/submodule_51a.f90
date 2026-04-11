program submodule_51_main
use submodule_51_mod
implicit none
real, parameter :: x(2,3) = reshape([1., 2., 3., 4., 5., 6.], [2, 3])
call show(x < 3.)
print *, "PASSED"
end program
