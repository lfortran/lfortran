program test_c_long_double_01
   use iso_c_binding
   implicit none

   real(c_long_double) :: x

   x = 1.0_c_long_double / 3.0_c_long_double

   print *, "kind      =", c_long_double
   print *, "size      =", c_sizeof(x)
!    print *, "precision =", precision(x)
   print *, "value     =", x

   if (c_sizeof(x) >= 8) then
      print *, "c_long_double: PASS"
   else
      print *, "c_long_double: FAIL"
      error stop
   end if
end program test_c_long_double_01
