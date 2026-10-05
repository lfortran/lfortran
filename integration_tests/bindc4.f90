program bindc4
use iso_c_binding, only: c_associated, c_loc, c_ptr, c_f_pointer, c_null_ptr, c_intptr_t
type(c_ptr) :: queries
type(c_ptr) :: queries2 = c_null_ptr
integer :: i, j
integer(2), target :: xv(3, 4), yv(3,4), zv(12)
integer :: newshape(2)
integer(2), pointer :: x(:, :), y(:,:)

newshape(1) = 2
newshape(2) = 3

x => xv
y => yv
queries = c_loc(zv)

do i = lbound(x, 1), ubound(x, 1)
    do j = lbound(x, 2), ubound(x, 2)
        print *, i, j, transfer(c_loc(x(i, j)), 0_c_intptr_t)
    end do
end do

call c_f_pointer(queries, x, newshape)
if (size(x, 1) /= 2 .or. size(x, 2) /= 3) error stop

print *, transfer(c_loc(x), 0_c_intptr_t), transfer(queries, 0_c_intptr_t)

do i = lbound(x, 1), ubound(x, 1)
    do j = lbound(x, 2), ubound(x, 2)
        print *, i, j, transfer(c_loc(x(i, j)), 0_c_intptr_t)
    end do
end do

call c_f_pointer(queries, x, [3, 4])
if (size(x, 1) /= 3 .or. size(x, 2) /= 4) error stop

print *, transfer(c_loc(x), 0_c_intptr_t), transfer(queries, 0_c_intptr_t)

do i = lbound(x, 1), ubound(x, 1)
    do j = lbound(x, 2), ubound(x, 2)
        print *, i, j, transfer(c_loc(x(i, j)), 0_c_intptr_t)
    end do
end do

if (.not. c_associated(queries, c_loc(x(1, 1)))) error stop
if (.not. c_associated(queries, c_loc(x))) error stop
if (c_associated(queries, c_loc(y))) error stop
if (c_associated(queries2)) error stop
if (.not. c_associated(queries)) error stop
queries = c_null_ptr

end program
