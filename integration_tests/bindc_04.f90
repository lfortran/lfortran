module thread_data_module
use, intrinsic :: iso_c_binding
type, bind(C) :: thread_data
type(c_ptr) :: a
integer(c_int) :: n
end type thread_data
end module thread_data_module

subroutine lcompilers_initialise_array(data) bind(C)
use thread_data_module
use iso_c_binding
implicit none
type(c_ptr), intent(in), value :: data
type(thread_data), pointer :: tdata
real(c_float), pointer :: a(:)
call c_f_pointer(data, tdata)

call c_f_pointer(tdata%a, a, [5])
print *, "Array address in fortran :", tdata%a
print *, "c_loc(a) :", c_loc(a)
print *, sum(a)
if (abs(sum(a) - 10.0) > 1.0e-8) error stop
end subroutine

subroutine b_func_fortran(data) bind(C)
use thread_data_module
use iso_c_binding
implicit none
type(c_ptr), intent(in), value :: data
real(c_float), pointer :: a(:)

call c_f_pointer(data, a, [5])
print *, "Array address in fortran b_func_fortran:", data
print *, "c_loc(a) b_func_fortran:", c_loc(a)
print *, sum(a)
if (abs(sum(a) - 10.0) > 1.0e-8) error stop
end subroutine

program bindc_04
use iso_c_binding
use thread_data_module
interface
subroutine lcompilers_initialise_array(data) bind(C)
import :: c_ptr
type(c_ptr), intent(in), value :: data
end subroutine

subroutine a_func(fn) bind(C, name="a_func")
import :: c_funptr
type(c_funptr), value :: fn
end subroutine

subroutine b_func_fortran(data) bind(C)
import :: c_ptr
type(c_ptr), intent(in), value :: data
end subroutine

subroutine b_func(fn) bind(C, name="b_func")
import :: c_funptr
type(c_funptr), value :: fn
end subroutine

subroutine c_func(arr) bind(C, name="c_func")
import :: c_ptr
type(c_ptr), value :: arr
end subroutine
end interface
type(thread_data), target :: tdata
real(c_float), dimension(:), pointer :: a

allocate(a(5))
a = [1.0, 2.0, 3.0, 4.0, 5.0]
tdata%a = c_loc(a)
tdata%n = 5

call a_func(c_funloc(lcompilers_initialise_array))
call b_func(c_funloc(b_func_fortran))
call c_func(c_loc(a))

end program bindc_04
