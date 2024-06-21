program main
   implicit none
   integer :: i1(5)
   integer :: b(5)

   i1 = [1, 2, 3, 4, 5]

   where(i1) b = 1
   print *, b
   if (all(b /= [1, 0, 1, 0, 1])) error stop

end program main
