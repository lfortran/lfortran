program arrays_intrin_01
   implicit none
   real, dimension(5) :: numbers = [1.5, -3.2, 4.5, 0.9, 7.2]
   
   print *, minval(numbers), maxval(numbers)

end program