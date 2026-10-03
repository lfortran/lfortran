module c_ptr_12_mod
use iso_c_binding, only: c_ptr, c_null_ptr
implicit none
contains

type(c_ptr) function make_null_ptr()
    make_null_ptr = c_null_ptr
end function

end module

program c_ptr_12
! Test that assigning a function returning type(c_ptr) to a type(c_ptr)
! variable works correctly.
use iso_c_binding, only: c_ptr, c_null_ptr, c_associated
use c_ptr_12_mod, only: make_null_ptr
implicit none

type(c_ptr) :: ptr

! Test assignment from a module function returning type(c_ptr)
ptr = make_null_ptr()
if (c_associated(ptr)) error stop

! Test assignment from an internal function returning type(c_ptr)
ptr = get_null_ptr()
if (c_associated(ptr)) error stop

print *, "ok"

contains

type(c_ptr) function get_null_ptr()
    get_null_ptr = c_null_ptr
end function

end program c_ptr_12
