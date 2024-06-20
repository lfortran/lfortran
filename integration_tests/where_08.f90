program main
   implicit none
   logical :: l1(5)
   logical :: l2(5)

   integer :: b(5)

   l1 = [.true., .false., .true., .false., .true.]
   l2 = [1 < 2, 2 == 2, .true., .false., 2 >= 1]

   where(l1) b = 1
   print *, b
   if (all(b /= [1, 0, 1, 0, 1])) error stop

   where(l2) b = 2
   print *, b
   if (all(b /= [2, 2, 2, 0, 2])) error stop

   where(get_logical_array()) b = 3
   print *, b
   if (all(b /= [3, 2, 3, 0, 3])) error stop

   where([.true., .false., .true., .false., .true.]) b = 4
   print *, b
   if (all(b /= [4, 2, 4, 0, 4])) error stop

contains
   function get_logical_array() result(value)
      implicit none
      logical :: value(5)
      value = [.true., .false., .true., .false., .true.]
   end function

end program main
