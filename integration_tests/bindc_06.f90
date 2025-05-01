module bindc_06_mod
   use iso_c_binding, only: c_ptr
   implicit none
   integer, bind(C, name="x") :: p
   ! type(c_ptr), bind(C, name="y") :: q
   integer(8), bind(C, name="z") :: r

   ! if (p /= 1) error stop
   ! print *, "r: ", r
   ! if (r /= 2) error stop
end module

program bindc_06
   use bindc_06_mod
   implicit none

   print *, "p: ", p
   if (p /= 1) error stop

   print *, "r: ", r
   if (r /= 2) error stop
end program bindc_06
