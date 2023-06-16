program bindc4
use iso_c_binding, only: c_associated, c_loc, c_ptr, c_f_pointer, c_null_ptr
type(c_ptr) :: queries
type(c_ptr) :: queries2 = c_null_ptr
integer :: i, j
integer(2), target :: xv(3, 4), yv(3,4)
integer :: newshape(2)
integer(2), pointer :: x(:, :), y(:,:)

newshape(1) = 2
newshape(2) = 3

x => xv
y => yv

do i = lbound(x, 1), ubound(x, 1)
    do j = lbound(x, 2), ubound(x, 2)
        print *, i, j, c_loc(x(i, j))
    end do
end do

call c_f_pointer(queries, x, newshape)

print *, c_loc(x), queries

do i = lbound(x, 1), ubound(x, 1)
    do j = lbound(x, 2), ubound(x, 2)
        print *, i, j, c_loc(x(i, j))
    end do
end do

call c_f_pointer(queries, x, [3, 4])

print *, c_loc(x), queries

do i = lbound(x, 1), ubound(x, 1)
    do j = lbound(x, 2), ubound(x, 2)
        print *, i, j, c_loc(x(i, j))
    end do
end do

if (c_associated(queries, c_loc(x(0, 0)))) error stop
if (.not. c_associated(queries, c_loc(x))) error stop
if (c_associated(queries, c_loc(y))) error stop
if (c_associated(queries2)) error stop
! TODO: Fix the following test
!if (.not. c_associated(queries)) error stop
queries = c_null_ptr

end program
