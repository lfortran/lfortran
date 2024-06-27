program where_11
   implicit none

   integer :: a(3)
   integer :: b(3)

   a = [1, 0, 1]
   b = [0, 0, 0]

   where (1 == a) b = 11
   print *, b
   if (all(b /= [11, 0, 11])) error stop

   where (1 == get_integer_array()) b = 22
   print *, b
   if (all(b /= [22, 0, 22])) error stop

   where (1 < get_integer_array()) b = 33
   print *, b
   if (all(b /= [22, 33, 22])) error stop

   where ([1, 2, 1] == 1) b = 44
   print *, b
   if (all(b /= [44, 33, 44])) error stop

   where (get_integer_array() > 1) b = 55
   print *, b
   if (all(b /= [44, 55, 44])) error stop

contains

   function get_integer_array() result(arr)
      implicit none
      integer :: arr(3)
      arr = [1, 2, 1]
   end function

end program where_11
