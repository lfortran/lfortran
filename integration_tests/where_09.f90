program where_09
   implicit none
   integer :: a(3)
   integer :: b(3)
   a = [1,0,-1]
   b = 0
   where(abs(a) == 1) b = 555
   print *, b
   if (b(1) /= 555) error stop
   if (b(2) /= 0) error stop
   if (b(3) /= 555) error stop
end program where_09
