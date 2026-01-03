subroutine x(dummy)
   real :: dummy
   print *, "Printed using subroutine call: ", dummy
   if (abs(dummy - 10.0) > 1e-7) error stop
   dummy = 5.0
 entry y(dummy)
   print *, "Printed using y entry statement: ", dummy
   if (abs(dummy - 5.0) > 1e-7) error stop
   dummy = 2.5
   return
 entry z(dummy)
   print *, "Printed using z entry statement: ", dummy
   if (abs(dummy - 2.5) > 1e-7) error stop
   dummy = 1.25
   return
 entry w(dummy)
   print *, "Printed using w entry statement: ", dummy
   if (abs(dummy - 1.25) > 1e-7) error stop
   return
end subroutine

program entry_03
   real :: dummy
   dummy = 10.0
   call x(dummy)
   dummy = 5.0
   call y(dummy)
   dummy = 2.5
   call z(dummy)
   dummy = 1.25
   call w(dummy)
end program
