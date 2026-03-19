program intrinsics_440
   real(8) :: y, x

   y = 1.0
   x = 1.0

   if (atan2d(y, x) /= 45) error stop
   if (atan2d(1.0, 0.0) /= 90) error stop
   if (atan2d(-1.0, 0.0) /= -90) error stop
   if (atan2d(0.0, 1.0) /= 0) error stop
   if (atan2d(0.0, -1.0) /= 180) error stop

end program intrinsics_440
