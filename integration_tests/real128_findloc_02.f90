program real128_findloc_02
! findloc of a real(16) array with an integer value converts the value
! to real(16) exactly, for both a constant and a variable value
use iso_fortran_env, only: real128, int64
implicit none
real(real128) :: a(4)
integer :: k(1), v
integer(int64) :: w
a = [1.0_real128, 3.0_real128, 2.0_real128, 3.0_real128]
k = findloc(a, 2)
if (k(1) /= 3) error stop 1
k = findloc(a, 3, back=.true.)
if (k(1) /= 4) error stop 2
k = findloc(a, 7)
if (k(1) /= 0) error stop 3
if (findloc(a, 3, 1) /= 2) error stop 4
v = 3
k = findloc(a, v)
if (k(1) /= 2) error stop 5
w = 2_int64
k = findloc(a, w)
if (k(1) /= 3) error stop 6
! 2**60 + 1 is not representable in double precision, but is in real(16)
a = [1.0_real128, 1152921504606846976.0_real128, &
    1152921504606846977.0_real128, 0.0_real128]
k = findloc(a, 1152921504606846977_int64)
if (k(1) /= 3) error stop 7
w = 1152921504606846977_int64
k = findloc(a, w)
if (k(1) /= 3) error stop 8
print *, "ok"
end program
