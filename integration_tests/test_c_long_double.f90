program test_c_long_double
   use iso_c_binding, only: c_long_double, c_int
   implicit none

   interface
      function add_long_double(a, b) bind(c, name="add_long_double")
         import :: c_long_double
         real(c_long_double), value :: a, b
         real(c_long_double) :: add_long_double
      end function add_long_double

      function check_long_double(val) bind(c, name="check_long_double")
         import :: c_long_double, c_int
         real(c_long_double), value :: val
         integer(c_int) :: check_long_double
      end function check_long_double
   end interface

   real(c_long_double) :: x, y, z
   integer(c_int) :: ok

   x = 1.234567890123456789_c_long_double
   y = 2.0_c_long_double
   z = add_long_double(x, y)

   print *, "c_long_double kind =", c_long_double
   print *, "x =", x
   print *, "x + 2 = ", z

   ok = check_long_double(z)

   if (ok == 1) then
      print *, "PASS: c_long_double C interop"
   else
      print *, "FAIL: c_long_double C interop"
      stop 1
   end if
end program test_c_long_double
