! A reshape assigned to an allocatable array reallocates it once, to the
! shape of the reshape, before it is copied into it: unallocated, of another
! shape, or of the same shape with other bounds, which it keeps (F2018
! 10.2.1.3), also in a loop, where the reallocation is done before it.
program realloc_lhs_reshape_01
implicit none
real, allocatable :: c(:,:)
real :: a(6), s
integer :: i

a = [1., 2., 3., 4., 5., 6.]
c = reshape(a, [2, 3])
if (any(shape(c) /= [2, 3]) .or. any(lbound(c) /= 1)) error stop 1
if (c(2, 3) /= 6) error stop 2
c = reshape(a(1:4), [2, 2])
if (any(shape(c) /= [2, 2]) .or. c(1, 2) /= 3) error stop 3
deallocate(c)
allocate(c(0:1, 0:1))
c = reshape(a(3:6), [2, 2])
if (any(lbound(c) /= 0) .or. c(1, 1) /= 6) error stop 4

s = 0
do i = 1, 100
    a(1) = real(mod(i, 7))
    c = reshape(a(1:4), [2, 2])
    s = s + c(0, 0) + c(1, 1)
end do
if (any(lbound(c) /= 0)) error stop 5
if (abs(s - (sum([(real(mod(i, 7)), i = 1, 100)]) + 400)) > 1e-3) error stop 6

deallocate(c)
do i = 1, 3
    c = reshape(a, [2, 3])
end do
if (any(shape(c) /= [2, 3]) .or. any(lbound(c) /= 1)) error stop 7
print *, "ok"
end program
