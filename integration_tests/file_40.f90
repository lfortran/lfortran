program blank_types_demo
   implicit none
   integer           :: i
   real              :: r

   open(10, file="data.txt", status="replace", form="formatted")
   write(10,'(A)') "1  2"    ! INTEGER field (I4)
   write(10,'(A)') "3 . 5"   ! REAL field (F5.1)
   close(10)

   open(10, file="data.txt", form="formatted", blank="null")

   read(10,'(I4)') i
   read(10,'(F5.1)') r

   close(10)
   print *, "INTEGER  :", i     ! -> 12
   print *, "REAL     :", r     ! -> 3.5

   if ( i /= 12) error stop
   if ( r /= 3.5) error stop

   open(10, file="data.txt", form="formatted", blank="zero")

   read(10,'(I4)') i
   read(10,'(F5.1)') r

   close(10)

   print *, "INTEGER  :", i     ! -> 1002
   print *, "REAL     :", r     ! -> 3.5   (same result)

   if ( i /= 1002) error stop
   if ( abs(r-30.0499992) > 1e-6 ) error stop

end program
