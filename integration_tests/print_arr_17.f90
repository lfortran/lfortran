! A print of an array still compiles and runs when --pass=print_arr
! explicitly requests the pass that the LLVM backend skips by default
program print_arr_17
implicit none
integer :: v(3) = [1, 2, 3]
print "(3i2)", v
print *, v
if (v(3) /= 3) error stop 1
end program
