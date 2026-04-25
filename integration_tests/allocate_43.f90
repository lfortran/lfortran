program allocate_43
use iso_c_binding, only: c_ptr, c_null_ptr, c_associated
implicit none

type :: my_type
    type(c_ptr) :: ptr
end type

type(my_type) :: obj
obj%ptr = c_null_ptr

call use_ptr(obj%ptr)
if (c_associated(obj%ptr)) error stop

contains

subroutine use_ptr(p)
    type(c_ptr), intent(inout) :: p
    p = c_null_ptr
end subroutine

end program allocate_43
