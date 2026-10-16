module real_star_01_mod
implicit none
contains

real*16 function twice16(a)
real*16, intent(in) :: a
twice16 = 2 * a
end function

end module

program real_star_01
use real_star_01_mod, only: twice16
implicit none
real*16 :: x, xs(3)
real*16, parameter :: c16 = 0.25e0_16
x = 0.5e-1_16
xs = [1.0e0_16, 2.0e0_16, 3.0e0_16]
print *, x
if (kind(x) /= 16) error stop
if (kind(xs) /= 16) error stop
if (storage_size(x) /= 128) error stop
if (precision(x) /= precision(1.0e0_16)) error stop
if (precision(x) <= precision(1.0d0)) error stop
if (abs(x - 0.05e0_16) > 1.0e-30_16) error stop
if (abs(twice16(x) - 0.1e0_16) > 1.0e-30_16) error stop
if (abs(sum(xs) + c16 - 6.25e0_16) > 1.0e-30_16) error stop
end program
