module shared_library_01_mod
implicit none
integer :: counter = 0
contains

integer function add_one(x) result(r)
integer, intent(in) :: x
counter = counter + 1
r = x + 1
end function

end module

subroutine shared_library_01_scale(x, n) bind(c, name="shared_library_01_scale")
use iso_c_binding, only: c_int
implicit none
integer(c_int), value :: n
integer(c_int), intent(inout) :: x(n)
x = 2*x
end subroutine
