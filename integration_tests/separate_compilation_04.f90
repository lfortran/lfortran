program separate_compilation_04
use separate_compilation_04b_module

implicit none
real, allocatable :: A(:,:)
allocate(A(3,3))
A = 02315.1235
call temp(A)
end program
