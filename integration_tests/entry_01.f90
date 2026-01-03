SUBROUTINE dzror()
   print *, "in dzror"
 ENTRY dstzr()
   print *, "in dztzr"
   RETURN
END

program main
   print *, "Call dzror"
   call dzror()
   print *, "Call dstzr"
   call dstzr()
end program
