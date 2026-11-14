program real128_findloc_01
use iso_fortran_env, only: real128
implicit none
real(real128) :: a(3)
integer :: k(1)
a = [0.5_real128, 1.5_real128, 2.5_real128]
k = findloc(a, 1.5)
print *, k
if (k(1) /= 2) error stop 1
k = findloc(a, 2.5d0)
if (k(1) /= 3) error stop 2
k = findloc(a, 7.5)
if (k(1) /= 0) error stop 3
end program
