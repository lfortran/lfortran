subroutine fn() bind(c)
print *, "Hello from fn in fortran"
end subroutine

subroutine test_val( val ) bind(c)
use iso_c_binding
integer(c_int), value :: val
print *, "val = ", val
if ( val /= 42 ) error stop
end subroutine

program bindc_03
use iso_c_binding
implicit none
interface
subroutine fn() bind(c)
end subroutine

subroutine test_val( val ) bind(c)
import :: c_int
integer(c_int), value :: val
end subroutine

subroutine execute_function( fn ) bind(c, name = "execute_function")
import :: c_funptr
type(c_funptr), value :: fn
end subroutine

subroutine execute_function_with_arg( fn, val ) bind(c, name = "execute_function_with_arg")
import :: c_funptr, c_int
integer(c_int), value :: val
type(c_funptr), value :: fn
end subroutine
end interface

call execute_function( c_funloc( fn ) )
call execute_function_with_arg( c_funloc( test_val ), 42 )
end program
