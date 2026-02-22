! Test for https://github.com/lfortran/lfortran/issues/3811 and #7775
! Read into character substring
program read_44
   implicit none
   character(10) :: string
   integer :: u

   string = 'ABCDEFGHIJ'
   open(newunit=u, status='scratch')
   write(u, '(A)') 'hello world'
   rewind(u)
   read(u, '(A)') string(1:6)
   close(u)

   if (string(1:6) /= 'hello ') error stop
   if (string(7:10) /= 'GHIJ') error stop
   print *, "PASS"
end program read_44
