program bindc_06
   use iso_c_binding, only: c_ptr
   implicit none
   integer, bind(C, name="x") :: p
   type(c_ptr), bind(C, name="y") :: q
end program bindc_06
