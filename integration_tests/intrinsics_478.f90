program intrinsics_478
   implicit none
   real :: y, x
   y = 1.0
   x = 1.0

   if (abs(atan(y, x) - 0.785398) > 1e-5) error stop 1
   if (abs(atand(y, x) - 45.0) > 1e-5) error stop 2
   if (abs(atanpi(y, x) - 0.25) > 1e-5) error stop 3

   if (abs(atan(y=y, x=x) - 0.785398) > 1e-5) error stop 4
   if (abs(atand(y=y, x=x) - 45.0) > 1e-5) error stop 5
   if (abs(atanpi(y=y, x=x) - 0.25) > 1e-5) error stop 6

   if (abs(atan(x=x) - 0.785398) > 1e-5) error stop 7
   if (abs(atand(x=x) - 45.0) > 1e-5) error stop 8
   if (abs(atanpi(x=x) - 0.25) > 1e-5) error stop 9

   if (abs(atan(x) - 0.785398) > 1e-5) error stop 10
   if (abs(atand(x) - 45.0) > 1e-5) error stop 11
   if (abs(atanpi(x) - 0.25) > 1e-5) error stop 12

   print *, "All tests passed"
end program intrinsics_478
