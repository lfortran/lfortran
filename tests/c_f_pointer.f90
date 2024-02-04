program c_f_pointer
   use iso_c_binding
   implicit none
   complex, target :: c_arr(2)
   real, pointer :: r_ptr(:)
   call c_f_pointer(c_loc(c_arr), r_ptr, shape=[4])
end program
