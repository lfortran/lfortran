program intrinsics_478
   implicit none
   real :: y, x
   y = 1.0
   x = 1.0

   if (abs(atan(y, x) - 0.785398) > 1e-5) error stop 1
   if (abs(atand(y, x) - 45.0) > 1e-5) error stop 2

   if (abs(atan(y=y, x=x) - 0.785398) > 1e-5) error stop 4
   if (abs(atand(y=y, x=x) - 45.0) > 1e-5) error stop 5
end program intrinsics_478
