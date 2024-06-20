program where_09
   implicit none
   integer :: a(3)
   integer :: b(3)

   a = [1,0,-1]
   b = 0

   where(abs(a) == 1) b = 555
   print *, b
   if (all(b /= [555, 0 , 555])) error stop

   where(abs(b) == (500 + 55)) b = 55
   print *, b
   if (all(b /= [55, 0 , 55])) error stop

   where(max(a, b) == 55) b = 3
   print *, b
   if (all(b /= [3, 0 , 3])) error stop

   where(max(b, [5, 2, 5]) == 5) b = 5
   print *, b
   if (all(b /= [5, 0 , 5])) error stop

end program where_09
