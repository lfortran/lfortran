module bindc_06_mod
   use iso_c_binding, only: c_ptr
   implicit none
   integer, bind(C, name="x") :: p
   logical, bind(C, name="c_logical") :: f_logical
   type(c_ptr), bind(C, name="y") :: q
   ! integer(8), bind(C, name="z") :: r
end module

program bindc_06
   use bindc_06_mod
   use iso_c_binding, only: c_f_pointer, c_int
   implicit none
   integer(c_int), pointer :: ptr_q

   print *, "p: ", p
   if (p /= 1) error stop

   ! this errors sometimes
   ! print *, "r: ", r
   ! if (r /= 2) error stop

   print *, "f_logical: ", f_logical
   if (f_logical /= .false.) error stop

   call c_f_pointer(q, ptr_q)
   print *, "ptr_q: ", ptr_q
   if (ptr_q /= 12) error stop
end program bindc_06
