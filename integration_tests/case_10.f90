program case_10
   implicit none
   integer :: i, result

   result = 0
   i = 1

   swtch: select case (i)
   case (1, 3) swtch
      result = result + 10
   case (2, 4) swtch
      result = result + 20
   case default swtch
      result = result + 30
   end select swtch

   if (result /= 10) error stop 1

   i = 2
   pick: select case (i)
   case (1) pick
      result = -1
   case default pick
      result = 99
   end select pick

   if (result /= 99) error stop 2

end program case_10
