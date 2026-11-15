! The reallocation of `c` is done once, before the loop that assigns it the
! same shape in every iteration; the reallocation of `d`, whose old size the
! loop reads first, stays in the loop.
program realloc_lhs_hoist_02
implicit none
real, allocatable :: a(:), b(:), c(:), d(:)
integer :: i, n, seen
a = [1.0, 2.0, 3.0]
b = [4.0, 5.0, 6.0]
d = [1.0]
n = 3
seen = 0
do i = 1, n
    a(1) = a(1) + 1
    c = a + b
    seen = seen + size(d)
    d = a
end do
if (size(c) /= 3) error stop 1
if (any(c /= [8.0, 7.0, 9.0])) error stop 2
if (seen /= 7) error stop 3
if (any(d /= [4.0, 2.0, 3.0])) error stop 4
print *, c
end program
