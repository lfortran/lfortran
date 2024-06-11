module thread_data_module
use, intrinsic :: iso_c_binding
type, bind(C) :: thread_data
type(c_ptr) :: a
end type thread_data
end module thread_data_module

subroutine lcompilers_initialise_array(data) bind(C)
use thread_data_module
use iso_c_binding
implicit none
type(c_ptr), value :: data
type(thread_data), pointer :: tdata
real(c_float), pointer :: a(:)
call c_f_pointer(data, tdata)

call c_f_pointer(tdata%a, a, [5])
print *, a
end subroutine

program bindc_04
use iso_c_binding
interface
subroutine lcompilers_initialise_array(data) bind(C)
import :: c_ptr
type(c_ptr), value :: data
end subroutine

subroutine a_func(fn) bind(C, name="a_func")
import :: c_funptr
type(c_funptr), value :: fn
end subroutine
end interface

call a_func(c_funloc(lcompilers_initialise_array))
end program bindc_04
