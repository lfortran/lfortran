module global_array_module
   implicit none
   integer(4), dimension(2) :: arr = [105, 109]
end module global_array_module

program test_global_array
   use global_array_module, only: arr
   implicit none
   integer :: i
   integer :: s
   i = 1
   print *, arr
   if (all(arr /= [105, 109])) error stop

   s = sum(arr)
   print *, s
   if (s /= 214) error stop

   print *, arr(i)
   if (arr(i) /= 105) error stop

   arr(i) = 210
   print *, arr(i)
   if (arr(i) /= 210) error stop

   print *, arr
   if (all(arr /= [210, 109])) error stop

end program test_global_array

