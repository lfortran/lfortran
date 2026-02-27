! Test for issue #10344
program cpp_pre_08
implicit none
integer :: a
#define get_x() 3
#define get_y() 7
! Test zero-argument macro expansion
if (get_x() /= 3) error stop "get_x() macro failed"
if (get_y() /= 7) error stop "get_y() macro failed"
! Test usage in expressions
a = get_x() + get_y()
if (a /= 10) error stop "expression macro expansion failed"
print *, "ok"
end program