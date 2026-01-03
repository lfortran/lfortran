program main
   implicit none
   integer  :: b(5)

   where(max(1.33, 2.67)) b = 12121
   print *, b
end program main
