program format_82
   implicit none
   integer :: u, ios
   real :: val
   character(len=100) :: line
   character(len=20)  :: round_val

   val = 2.67

   ! -------------------------------
   ! round="up"
   ! -------------------------------
   open(newunit=u, file="format_82_file.txt", status="replace", &
      form="formatted", round="up")
   write(u, '(F4.1)') val
   close(u)

   open(newunit=u, file="format_82_file.txt", status="old")
   read(u, '(A)') line
   close(u, status="delete")

   if (trim(adjustl(line)) /= "2.7") error stop

   ! -------------------------------
   ! round="down"
   ! -------------------------------
   open(newunit=u, file="format_82_file.txt", status="replace", &
      form="formatted", round="down")
   write(u, '(F4.1)') val
   close(u)

   open(newunit=u, file="format_82_file.txt", status="old")
   read(u, '(A)') line
   close(u, status="delete")

   if (trim(adjustl(line)) /= "2.6") error stop

   ! -------------------------------
   ! round="nearest"
   ! -------------------------------
   open(newunit=u, file="format_82_file.txt", status="replace", &
      form="formatted", round="nearest")
   write(u, '(F4.1)') val
   close(u)

   open(newunit=u, file="format_82_file.txt", status="old")
   read(u, '(A)') line
   close(u, status="delete")

   if (trim(adjustl(line)) /= "2.7") error stop

   ! -------------------------------
   ! round="zero"
   ! -------------------------------
   open(newunit=u, file="format_82_file.txt", status="replace", &
      form="formatted", round="zero")
   write(u, '(F4.1)') val
   close(u)

   open(newunit=u, file="format_82_file.txt", status="old")
   read(u, '(A)') line
   close(u, status="delete")

   if (trim(adjustl(line)) /= "2.6") error stop

   ! Test 1: nearest
   open(newunit=u, file="format_82_file.txt", status="replace", &
      form="formatted", round="nearest")
   inquire(unit=u, round=round_val)
   if (trim(round_val) /= "NEAREST") error stop 1
   close(u, status="delete")

   ! Test 2: up
   open(newunit=u, file="format_82_file.txt", status="replace", &
      form="formatted", round="up")
   inquire(unit=u, round=round_val)
   if (trim(round_val) /= "UP") error stop 2
   close(u, status="delete")

   ! Test 3: down
   open(newunit=u, file="format_82_file.txt", status="replace", &
      form="formatted", round="down")
   inquire(unit=u, round=round_val)
   if (trim(round_val) /= "DOWN") error stop 3
   close(u, status="delete")

   ! Test 4: zero
   open(newunit=u, file="format_82_file.txt", status="replace", &
      form="formatted", round="zero")
   inquire(unit=u, round=round_val)
   if (trim(round_val) /= "ZERO") error stop 4
   close(u, status="delete")

   ! Test 5: compatible
   open(newunit=u, file="format_82_file.txt", status="replace", &
      form="formatted", round="compatible")
   inquire(unit=u, round=round_val)
   if (trim(round_val) /= "COMPATIBLE") error stop 5
   close(u, status="delete")

   ! Test 6: processor_defined
   open(newunit=u, file="format_82_file.txt", status="replace", &
      form="formatted", round="processor_defined")
   inquire(unit=u, round=round_val)
   if (trim(round_val) /= "PROCESSOR_DEFINED") error stop 6
   close(u, status="delete")

   ! Test 7: default
   open(newunit=u, file="format_82_file.txt", status="replace", &
      form="formatted")
   inquire(unit=u, round=round_val)
   if (trim(round_val) /= "PROCESSOR_DEFINED") error stop 7
   close(u, status="delete")

end program format_82
