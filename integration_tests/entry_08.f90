subroutine dzror(status)
   integer status
 entry dstzr()
   return
   status = 8
end subroutine

program entry_08
   integer :: status
   status = 1
   call dzror(status)
   if (status /= 1) error stop
   print *, "status = ", status
end program
