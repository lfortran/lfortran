program main
   implicit none
   integer  :: b(5)

   where(.true.) b = 12121
   print *, b
end program main
