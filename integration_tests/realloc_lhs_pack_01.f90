! An array allocated with the extents of another one has its shape: an
! assignment of an expression of the shape of that one to it needs no
! reallocation, as the temporary of the mask of a pack does not; one of
! another shape still reallocates it. pack gives its target the number of
! true elements of the mask.
program realloc_lhs_pack_01
implicit none
real, allocatable :: a(:), b(:), c(:), t(:)
real, allocatable :: m(:,:), u(:,:)
integer :: i, k

a = [1., -2., 3., -4., 5.]
b = [1., 2.]
allocate(t(size(a, 1)))
t = 2*a
if (size(t) /= 5 .or. t(5) /= 10) error stop 1
t = b
if (size(t) /= 2 .or. any(t /= b)) error stop 2

allocate(m(2, 3))
allocate(u(size(m, 1), size(m, 2)))
m = 1
u = m + 1
if (any(shape(u) /= [2, 3]) .or. any(u /= 2)) error stop 3
deallocate(u)
allocate(u(size(m, 2), size(m, 1)))
u = transpose(m)
if (any(shape(u) /= [3, 2])) error stop 4

k = 0
do i = 1, 5
    c = pack(a, a > real(i - 3))
    k = k + size(c)
    if (size(c) > 0) then
        if (c(1) <= real(i - 3)) error stop 5
    end if
end do
! true elements: 3, 3, 3, 2, 2
if (k /= 13) error stop 6
c = pack(a, a > 100.)
if (size(c) /= 0) error stop 7
print *, "ok"
end program
