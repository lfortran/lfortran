program capturing_c_callback
use iso_c_binding
implicit none
integer :: captured
type(c_funptr) :: address
captured = 7
address = c_funloc(callback)
contains
integer(c_int) function callback(x) bind(c)
    integer(c_int), value :: x
    callback = x + captured
end function
end program
