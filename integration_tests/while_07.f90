program while_07
   implicit none
   logical :: finished(4)
   integer :: count = 0
   finished = .true.

   do while (all(finished))
      finished = .false.
      count = count + 1
   end do
   print *, count
   if (count > 1) error stop

end program while_07
