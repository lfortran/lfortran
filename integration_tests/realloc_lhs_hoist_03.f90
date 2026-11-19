! Reallocation on assignment of array expressions over sections inside DO
! loops: the shape of the value is the shape of the sections, which the
! subscripts of the sections do not change.
program realloc_lhs_hoist_03
implicit none
real, allocatable :: m(:,:), v(:), t(:), u(:), w(:), t2(:,:)
integer :: i, j, n, lo, sizes(4)

allocate(m(4, 3))
do j = 1, 3
    do i = 1, 4
        m(i, j) = 10 * i + j
    end do
end do

! A column of a matrix in a loop over the columns.
do j = 1, size(m, 2)
    t = m(:, j) * 2
    if (any(t /= 2 * m(:, j))) error stop 1
end do
if (size(t) /= 4 .or. lbound(t, 1) /= 1) error stop 2

! The same in a loop nest, with the section changing the array it is a
! column of.
do i = 1, 2
    do j = 1, size(m, 2)
        t = m(:, j) * 2
        m(1, j) = t(1)
    end do
end do
if (any(m(1, :) /= [44.0, 48.0, 52.0])) error stop 3

! Rows, with a stride: the extent is max(0, (u - l + s) / s).
v = [1, 2, 3, 4, 5, 6, 7]
n = size(v)
do i = 1, 3
    t = v(1:n:2) + i
    u = v(n:1:-3) * i
    w = m(i, 1:3:2)
end do
if (size(t) /= 4 .or. any(t /= [4.0, 6.0, 8.0, 10.0])) error stop 4
if (size(u) /= 3 .or. any(u /= [21.0, 12.0, 3.0])) error stop 5
if (size(w) /= 2 .or. any(w /= m(3, [1, 3]))) error stop 6

! An empty section.
do i = 1, 2
    t = v(3:2) + i
end do
if (size(t) /= 0) error stop 7

! A two-dimensional section.
do i = 1, 2
    t2 = m(2:3, :) + i
end do
if (any(shape(t2) /= [2, 3])) error stop 8
if (any(t2 /= m(2:3, :) + 2)) error stop 9

! The section depends on the loop variable.
do i = 1, 4
    t = v(i:4) + 1
    sizes(i) = size(t)
end do
if (any(sizes /= [4, 3, 2, 1])) error stop 10

! A bound of the section changes in the loop.
lo = 0
do i = 1, 4
    lo = lo + 1
    t = v(lo:5) * 2
    sizes(i) = size(t)
end do
if (any(sizes /= [5, 4, 3, 2])) error stop 11

! The array of the section is reallocated in the loop, changing its bounds.
deallocate(m)
do i = 1, 3
    allocate(m(i + 1, 2))
    m = i
    t = m(:, 2) + 1
    sizes(i) = size(t)
    deallocate(m)
end do
if (any(sizes(1:3) /= [2, 3, 4])) error stop 12
if (any(t /= 4)) error stop 13

! A loop that does not run.
deallocate(t)
n = 0
do i = 1, n
    t = v(1:3) * 2
end do
if (allocated(t)) error stop 14

print *, "ok"
end program
