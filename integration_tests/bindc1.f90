program bindc1
use iso_c_binding, only: c_loc, c_ptr
integer, pointer :: x
type(c_ptr) :: p
p = c_loc(x)
end program
