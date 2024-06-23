program where_08
   implicit none
   logical :: l1(5)
   logical :: l2(5)

   logical, allocatable, dimension(:) :: l3
   logical, allocatable, dimension(:) :: l4

   integer :: b(5)

   l1 = [.true., .false., .true., .false., .true.]
   l2 = [1 < 2, 2 == 2, .true., .false., 2 >= 1]

   allocate(l3(5))
   l3 = [.true., .false., .false., .true., .false.]
   
   allocate(l4(5))
   l4 = .true.

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

   where(l3) b = 5
   print *, b
   if (all(b /= [5, 2, 4, 5, 4])) error stop

   where(l4) b = 6
   print *, b
   if (all(b /= [6, 6, 6, 6, 6])) error stop

contains
   function get_logical_array() result(value)
      implicit none
      logical :: value(5)
      value = [.true., .false., .true., .false., .true.]
   end function

end program where_08