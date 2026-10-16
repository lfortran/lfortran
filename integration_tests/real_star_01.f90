module real_star_01_mod
implicit none
contains

real*16 function twice16(a)
real*16, intent(in) :: a
twice16 = 2 * a
end function

real*10 function twice10(a)
real*10, intent(in) :: a
twice10 = 2 * a
end function

end module

program real_star_01
use real_star_01_mod, only: twice10, twice16
implicit none
real*16 :: x, xs(3)
real*10 :: y
real*16, parameter :: c16 = 0.25e0_16
x = 0.5e-1_16
y = 1.5e0_10
xs = [1.0e0_16, 2.0e0_16, 3.0e0_16]
print *, x, y
if (kind(x) /= 16) error stop
if (kind(y) /= 10) error stop
if (kind(xs) /= 16) error stop
if (storage_size(x) /= 128) error stop
if (precision(x) /= precision(1.0e0_16)) error stop
if (precision(y) /= precision(1.0e0_10)) error stop
if (precision(x) <= precision(1.0d0)) error stop
if (abs(x - 0.05e0_16) > 1.0e-30_16) error stop
if (abs(twice16(x) - 0.1e0_16) > 1.0e-30_16) error stop
y = twice10(y)
if (y < 2.9999e0_10 .or. y > 3.0001e0_10) error stop
if (abs(sum(xs) + c16 - 6.25e0_16) > 1.0e-30_16) error stop
end program
