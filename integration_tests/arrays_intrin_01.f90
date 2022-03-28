program arrays_intrin_01
   implicit none
   integer, dimension(11) :: nums_int = [5, 4, 3, 2, 1, 0, -1, -2, -3, -4, -5]
   real, dimension(5) :: nums_real = [1.5, -3.2, 4.5, 0.9, 7.2]
   
   if (minval(nums_int) /= -5) error stop
   if (maxval(nums_int) /= 5) error stop
   if (minval(nums_real) /= -3.2) error stop
   if (maxval(nums_real) /= 7.2) error stop
   
   print *, minval(nums_int), maxval(nums_int)
   print *, minval(nums_real), maxval(nums_real)

end program
