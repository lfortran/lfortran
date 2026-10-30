module capturing_c_callback_dynamic_module
use iso_c_binding
implicit none
abstract interface
    integer(c_int) function callback_interface(x) bind(c)
        import c_int
        integer(c_int), value :: x
    end function
end interface
contains
function export(f) result(address)
    procedure(callback_interface) :: f
    type(c_funptr) :: address
    address = c_funloc(f)
end function
end module
program capturing_c_callback_dynamic
use capturing_c_callback_dynamic_module
implicit none
integer :: captured
type(c_funptr) :: address
captured = 7
address = export(callback)
contains
integer(c_int) function callback(x) bind(c)
    integer(c_int), value :: x
    callback = x + captured
end function
end program
