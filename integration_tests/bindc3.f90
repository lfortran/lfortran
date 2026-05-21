program bindc3
use iso_c_binding, only: c_loc, c_ptr, c_f_pointer, c_intptr_t
type(c_ptr) :: queries
integer :: idx = 1
integer(2), pointer :: x
integer(2), target :: y
call c_f_pointer(queries, x)
print *, transfer(c_loc(x), 0_c_intptr_t), transfer(queries, 0_c_intptr_t)
x => y
print *, transfer(c_loc(x), 0_c_intptr_t), transfer(c_loc(y), 0_c_intptr_t)
end program
