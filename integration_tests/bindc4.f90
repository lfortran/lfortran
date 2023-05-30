program bindc4
use iso_c_binding, only: c_associated, c_loc, c_ptr, c_f_pointer
type(c_ptr) :: queries
integer :: i, j
integer(2), target :: xv(3, 4)
integer :: newshape(2)
integer(2), pointer :: x(:, :)

newshape(1) = 2
newshape(2) = 3

x => xv

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
end program
