program separate_compilation_05
use separate_compilation_05b_module
use ieee_arithmetic, only: ieee_is_nan


implicit none
real, allocatable :: A(:,:)
allocate(A(3,3))
A = 02315.1235
call temp(A)
print *, ieee_is_nan(A(1,1))
if ( ieee_is_nan(A(1,1)) ) error stop
end program
