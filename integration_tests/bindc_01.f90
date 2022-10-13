! Tests c_ptr, c_f_pointer, c_loc
program bindc_01
use iso_c_binding, only: c_ptr, c_loc, c_f_pointer
use bindc_01b, only: ret_ptr, print_ptr
implicit none
type(c_ptr) :: cptr
real, pointer :: a(:)
real, target :: b(3)
integer :: i
call ret_ptr(cptr)
call c_f_pointer(cptr, a, [12])
do i = 1, size(a)
    a(i) = i
end do
call print_ptr(12, c_loc(a))
do i = 1, size(b)
    b(i) = 10 + i
end do
call print_ptr(3, c_loc(b))
end
