program string_slice2
   implicit none
   character(len=8) :: s = "lfortran"

   print*, "Length of s:", len(s)
   print*, s(1: 9)

end program