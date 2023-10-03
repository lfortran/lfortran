program arrays_31
   implicit none

   integer :: x(5)
   integer :: pqr = size(x)

   print *, pqr
   if (pqr /= 5) error stop

end program
