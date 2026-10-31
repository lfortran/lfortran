module c_ptr_26_mod
use iso_c_binding, only: c_ptr, c_loc
implicit none
contains
    type(c_ptr) function get_ptr_in(x) result(r)
    integer, target, intent(in) :: x
    r = c_loc(x)
    end function

    type(c_ptr) function get_ptr_no_intent(x) result(r)
    integer, target :: x
    r = c_loc(x)
    end function
end module

program c_ptr_26
use iso_c_binding, only: c_ptr, c_loc, c_associated
use c_ptr_26_mod
implicit none
integer, target :: j = 7
type(c_ptr) :: p
p = get_ptr_in(j)
if (.not. c_associated(p, c_loc(j))) error stop 1
p = get_ptr_no_intent(j)
if (.not. c_associated(p, c_loc(j))) error stop 2
print *, "ok"
end program
