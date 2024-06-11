! Tests c_ptr, c_f_pointer, c_loc
program bindc_01
use iso_c_binding, only: c_ptr, c_loc, c_f_pointer
use bindc_01b, only: ret_ptr, ret_ptr1, print_ptr
implicit none
type(c_ptr) :: cptr
type(c_ptr) :: cptr1
real, pointer :: a(:)
real, pointer :: a1(:)
real, target :: b(3)
integer :: i
call ret_ptr(cptr)
call c_f_pointer(cptr, a, [12])
do i = 1, size(a)
    a(i) = i
end do

call print_ptr(12, c_loc(a))

call ret_ptr1(cptr1)
call c_f_pointer(cptr1, a1, [12])
do i = 1, size(a1)
    a1(i) = i
end do

call print_ptr(12, c_loc(a1))

do i = 1, size(b)
    b(i) = 10 + i
end do
call print_ptr(3, c_loc(b))
end
