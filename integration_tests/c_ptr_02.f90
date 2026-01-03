program test_c_ptr_02
   use iso_c_binding
   implicit none
   complex, target :: c_arr(2)
   real, pointer :: r_ptr(:)

   type :: test_obj
      type(c_ptr) :: ptr = c_NULL_ptr
   end type test_obj
   type(test_obj) :: instance
   type(c_ptr) :: temp_ptr
   temp_ptr = instance%ptr

   c_arr = (1, 2)
   call c_f_pointer(c_loc(c_arr), r_ptr, shape=[4])
   print *, c_arr
   print *, r_ptr
   if (c_associated(temp_ptr) .neqv. .false.) error stop
end program
