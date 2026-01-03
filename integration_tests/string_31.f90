program string_31
   implicit none
   character(len=8) :: s = "lfortran"
   character(len=5) :: n

   print*, "Length of s:", len(s)

   print*, "Whole string:", s(1:len(s))
   if ( s(1:len(s)) /= "lfortran" ) error stop

   print*, "Slice of string (2:5):", s(1:-1)
   if ( s(1:-1) /= "" ) error stop

   print*, "Slice of string (2:5):", s(2:5)
   if ( s(2:5) /= "fort" ) error stop

   n = "12345"
   print *, any(n(2:2) == ["3", "2"])
   if ( .not. any(n(2:2) == ["3", "2"]) ) error stop

end program
