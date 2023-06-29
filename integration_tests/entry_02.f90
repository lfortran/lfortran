subroutine x(dummy)
   real :: dummy
   print *, "Printed using subroutine call: ", dummy
   if (abs(dummy - 10.0) > 1e-7) error stop
   dummy = 5.0
 entry y(dummy)
   print *, "Printed using entry statement: ", dummy
   if (abs(dummy - 5.0) > 1e-7) error stop
   return
end subroutine

program entry_02
   real :: dummy
   dummy = 10.0
   call x(dummy)
   dummy = 5.0
   call y(dummy)
end program
