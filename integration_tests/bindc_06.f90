! this tests module variables with bindC are
! correctly assigned in the C file
module bindc_06_mod
   use iso_c_binding, only: c_ptr
   implicit none
   integer, bind(C, name="c_int4") :: f_int4
   logical, bind(C, name="c_logical") :: f_logical
   type(c_ptr), bind(C, name="c_type_c_ptr") :: f_type_c_ptr
   integer(8), bind(C, name="c_int8") :: f_int8
end module

program bindc_06
   use bindc_06_mod
   use iso_c_binding, only: c_f_pointer, c_int
   implicit none
   integer(c_int), pointer :: ptr_f_type_c_ptr

   print *, "f_int4: ", f_int4
   if (f_int4 /= 1) error stop

   print *, "f_int8: ", f_int8
   if (f_int8 /= 2) error stop

   print *, "f_logical: ", f_logical
   if (f_logical /= .false.) error stop

   call c_f_pointer(f_type_c_ptr, ptr_f_type_c_ptr)
   print *, "ptr_f_type_c_ptr: ", ptr_f_type_c_ptr
   if (ptr_f_type_c_ptr /= 12) error stop

end program bindc_06
