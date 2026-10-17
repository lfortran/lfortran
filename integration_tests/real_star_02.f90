module real_star_02_mod
implicit none
contains

real*10 function twice10(a)
real*10, intent(in) :: a
twice10 = 2 * a
end function

end module

program real_star_02
! real*10 declarations, function prefix and IMPLICIT statement
use real_star_02_mod, only: twice10
implicit real*10 (r)
real*10 :: y
y = 1.5e0_10
r = 2.5e0_10
print *, y, r
if (kind(y) /= 10) error stop
if (kind(r) /= 10) error stop
if (precision(y) /= precision(1.0e0_10)) error stop
y = twice10(y)
if (y < 2.9999e0_10 .or. y > 3.0001e0_10) error stop
if (r < 2.4999e0_10 .or. r > 2.5001e0_10) error stop
end program
