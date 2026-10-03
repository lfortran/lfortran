program test_c_f_procpointer
   use iso_c_binding, only: c_funptr, c_funloc, c_f_procpointer
   implicit none
   
   abstract interface
      subroutine sub_interface(x) bind(c)
         use iso_c_binding
         integer(c_int), value :: x
      end subroutine
   end interface
   
   procedure(sub_interface), pointer :: f_proc
   type(c_funptr) :: c_proc
   
   interface
      subroutine c_callback(x) bind(c, name='c_callback')
         use iso_c_binding
         integer(c_int), value :: x
      end subroutine
   end interface
   
   ! Get C function pointer using c_funloc
   c_proc = c_funloc(c_callback)
   
   ! Convert to Fortran procedure pointer
   call c_f_procpointer(c_proc, f_proc)
   
   ! Call C function via Fortran procedure pointer
   call f_proc(42)
   print *, "test passed"

end program test_c_f_procpointer
