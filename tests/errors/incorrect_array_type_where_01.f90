program main
   implicit none
   integer :: b(5)

   where([1, 2, 3, 4, 5]) b = 1
   print *, b
   if (all(b /= [1, 0, 1, 0, 1])) error stop

end program main