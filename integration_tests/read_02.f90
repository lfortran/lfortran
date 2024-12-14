program read_02
   implicit none
   integer :: i
   character(len=10) :: tmp
   integer :: u
   open(newunit=u, file="read_02_data.txt")

   read(u, '(A)', iostat=i) tmp
   print *, i, tmp, len(tmp)
   if (trim(tmp) /= "aa") error stop

   read(u, '(A)', iostat=i) tmp
   print *, i, tmp, len(tmp)
   if (trim(tmp) /= "bb") error stop

   read(u, '(A)', iostat=i) tmp
   print *, i, tmp, len(tmp)
   if (trim(tmp) /= "cc") error stop

   read(u, '(A)', iostat=i) tmp
   print *, i, tmp, len(tmp)
   if (trim(tmp) /= "dd") error stop

   read(u, '(A)', iostat=i) tmp
   print *, i, tmp, len(tmp)
   if (trim(tmp) /= "ee") error stop


   close(u)
end program

