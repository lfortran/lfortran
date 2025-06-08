program main
   implicit none

   integer :: i

   ! Loop 1
   do i = 1, 2
      print *,'In loop 1 i =', i
      if (i < 2) exit
   end do
   print *,'After loop 1 i = ', i
   if (i /= 1) error stop

   ! Loop 2
   print *
   do i = 1, 3
      print *,'In loop 2 i = ', i
      if (i < 2) then
         print *, "if (i < 2) then"
      else
         exit
      end if
   end do
   print *,'After loop 2 i = ', i
   if (i /= 2) error stop


   ! Loop 3
   print *
   do i = 1, 6
      print *,'In loop 3 i = ', i
      if (i <= 4) then
         if (i - 1 == 3) then
            if (i - 1 == 2) exit
            exit
         end if
      end if
   end do
   print *,'After loop 3 i = ', i
   if (i /= 4) error stop

   ! Loop 4
   print *
   do i = 1, 6
      print *,'In loop 4 i = ', i
      if (i <= 4) then
         if (i - 1 == 3) then
            if (i - 1 == 2) exit
            exit
         else
            print *, "i - 1 /= 3"
         end if
      end if
   end do
   print *,'After loop 4 i = ', i
   if (i /= 4) error stop

end program
