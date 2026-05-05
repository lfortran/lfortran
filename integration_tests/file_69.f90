program file_69
   implicit none

   character(128) :: scr_fn
   integer, parameter :: lun_scr = 42

   open (lun_scr, status='scratch', form='formatted')
   inquire (lun_scr, name=scr_fn)
   print *, 'scratch filename = ', trim (scr_fn)
   write (lun_scr, *) 'hello world!'

end program
