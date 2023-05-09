program example
use, intrinsic :: iso_c_binding, only: c_ptr
type(c_ptr) :: c_ptr_1
if (c_associated(c_ptr_1)) then
    print *, "The variable is a C pointer"
else
    error stop
end if
end program example
