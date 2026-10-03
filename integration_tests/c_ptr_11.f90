program c_ptr_11
use iso_c_binding, only: c_ptr, c_null_ptr, c_associated
implicit none

type :: wrapper
    type(c_ptr) :: ptr
end type

type(wrapper) :: w
w%ptr = c_null_ptr

call check_ptr(w%ptr)
if (c_associated(w%ptr)) error stop

call modify_ptr(w%ptr)
if (c_associated(w%ptr)) error stop

contains

subroutine check_ptr(p)
    type(c_ptr), intent(in) :: p
    if (c_associated(p)) error stop
end subroutine

subroutine modify_ptr(p)
    type(c_ptr), intent(inout) :: p
    p = c_null_ptr
end subroutine

end program c_ptr_11
