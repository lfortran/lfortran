program file_close_02
   ! Test that CLOSE(STATUS='DELETE') with an uppercase status value
   ! correctly deletes the file. The value of STATUS is case-insensitive
   ! per the Fortran standard.
   implicit none
   character(len=16) :: file_name
   integer :: u = 10
   logical :: ex

   file_name = "file_close_02.txt"

   ! Uppercase status string literal
   open(unit=u, file=file_name, status='replace')
   close(unit=u, status='DELETE')
   inquire(file=file_name, exist=ex)
   print *, "deleted (uppercase literal)?", .not. ex
   if (ex) error stop

   ! Mixed case status string literal
   open(unit=u, file=file_name, status='replace')
   close(unit=u, status='Delete')
   inquire(file=file_name, exist=ex)
   print *, "deleted (mixed-case literal)?", .not. ex
   if (ex) error stop

   ! Lowercase status string literal (already worked)
   open(unit=u, file=file_name, status='replace')
   close(unit=u, status='delete')
   inquire(file=file_name, exist=ex)
   print *, "deleted (lowercase literal)?", .not. ex
   if (ex) error stop
end program file_close_02
