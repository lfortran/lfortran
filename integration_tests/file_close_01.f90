program file_close_01
   implicit none
   character(:),allocatable:: file_name
   character(len=16) :: str
   character(len=6) :: st = "delete"
   integer :: newu = 10
   logical :: ex

   file_name = "file_close_1.txt"
   open(newunit=newu,file=file_name)
   write(newu, *) file_name       ! file_name now has content = its own name
   rewind(newu)
   read(newu,  *) str
   print *, "contents: ", str
   close(newu,status='delete')
   inquire(file=file_name,exist=ex)
   print "(3A,L2)",'Was ',file_name,' deleted?', .not. ex
   if (ex .neqv. .false.) error stop


   open(newunit=newu,file=file_name)
   close(newu,status=st)   ! status specified through variable
   inquire(file=file_name,exist=ex)
   print "(3A,L2)",'Was ',file_name,' deleted?', .not. ex
   if (ex .neqv. .false.) error stop
end program file_close_01
