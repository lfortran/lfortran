! Overlapping (aliasing) array assignment inside a `do concurrent`.
!
! Fortran requires the right-hand side of an array assignment to be
! evaluated as if it were fully read before anything is assigned to the
! left-hand side. When both sides designate overlapping storage of the
! same array, an ascending element-by-element copy violates that, because
! it reads elements the same statement has already overwritten.
program gpu_metal_233
implicit none
real :: a(5), b(6), c(5), d(5), e(5)
real :: ap(2,5), src(2,5)
integer :: i, k, n

n = 1

! Rank-1 full reversal in place.
a = [(real(i), i = 1, 5)]
do concurrent (k = 1:n)
    a(:) = a(5:1:-1)
end do
do i = 1, 5
    if (abs(a(i) - real(6 - i)) > 1.0e-6) error stop "rank-1 reversal"
end do

! Whole-array target with a reversed section source.
e = [(real(i), i = 1, 5)]
do concurrent (k = 1:n)
    e = e(5:1:-1)
end do
do i = 1, 5
    if (abs(e(i) - real(6 - i)) > 1.0e-6) error stop "whole-array reversal"
end do

! Forward-shift overlap: b(3:6) = b(2:5).
b = [(real(i), i = 1, 6)]
do concurrent (k = 1:n)
    b(3:6) = b(2:5)
end do
if (abs(b(1) - 1.0) > 1.0e-6) error stop "forward shift b(1)"
if (abs(b(2) - 2.0) > 1.0e-6) error stop "forward shift b(2)"
do i = 3, 6
    if (abs(b(i) - real(i - 1)) > 1.0e-6) error stop "forward shift"
end do

! Rank-2 in-place column reversal, as `negate_and_flip` does.
do i = 1, 5
    ap(1,i) = real(i)
    ap(2,i) = real(10 + i)
end do
do concurrent (k = 1:5)
    ap(:,k) = ap(2:1:-1,k)
end do
do i = 1, 5
    if (abs(ap(1,i) - real(10 + i)) > 1.0e-6) error stop "column flip row 1"
    if (abs(ap(2,i) - real(i)) > 1.0e-6) error stop "column flip row 2"
end do

! Fence: backward-shift overlap, which an ascending copy gets right.
c = [(real(i), i = 1, 5)]
do concurrent (k = 1:n)
    c(1:4) = c(2:5)
end do
do i = 1, 4
    if (abs(c(i) - real(i + 1)) > 1.0e-6) error stop "backward shift"
end do
if (abs(c(5) - 5.0) > 1.0e-6) error stop "backward shift tail"

! Fence: elementwise self-reference is not an overlap.
d = [(real(i), i = 1, 5)]
do concurrent (k = 1:n)
    d(:) = d(:) + 1.0
end do
do i = 1, 5
    if (abs(d(i) - real(i + 1)) > 1.0e-6) error stop "elementwise self"
end do

! Fence: a reversed section of a different array needs no temporary.
do i = 1, 5
    src(1,i) = real(i)
    src(2,i) = real(10 + i)
end do
do concurrent (k = 1:5)
    ap(:,k) = src(2:1:-1,k)
end do
do i = 1, 5
    if (abs(ap(1,i) - real(10 + i)) > 1.0e-6) error stop "different arrays row 1"
    if (abs(ap(2,i) - real(i)) > 1.0e-6) error stop "different arrays row 2"
end do

! Fence: the same reversal outside a `do concurrent`.
a = [(real(i), i = 1, 5)]
a(:) = a(5:1:-1)
do i = 1, 5
    if (abs(a(i) - real(6 - i)) > 1.0e-6) error stop "reversal outside loop"
end do

print *, "ok"
end program gpu_metal_233
