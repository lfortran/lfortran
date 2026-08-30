program gpu_metal_230
! Strided array sections inside a DO CONCURRENT offloaded to the GPU.
! The element loop that lowers an array-section assignment substituted
! the loop counter directly for a range dimension, discarding that
! dimension's lower bound and step. `a(4:1:-1)` was therefore read as
! `a(1:4)` -- a silent wrong answer with no diagnostic.
implicit none
integer, parameter :: n = 8
real :: a(n), b(n), d(4)
real :: p(4,4), q(4,4)
integer :: ia(n), ib(4)
integer :: j, k

do j = 1, n
    a(j) = real(j)
    ia(j) = j
end do
b = 0.0
d = 0.0
ib = 0
do j = 1, 4
    do k = 1, 4
        p(j,k) = real(10*j + k)
    end do
end do
q = 0.0

! reversed unit stride
do concurrent (j = 1:1)
    d(1:4) = a(4:1:-1)
end do
do j = 1, 4
    if (abs(d(j) - real(5-j)) > 1.0e-6) error stop "reversed unit stride wrong"
end do

! positive non-unit stride
d = 0.0
do concurrent (j = 1:1)
    d(1:4) = a(1:7:2)
end do
do j = 1, 4
    if (abs(d(j) - real(2*j-1)) > 1.0e-6) error stop "positive stride wrong"
end do

! reversed non-unit stride
d = 0.0
do concurrent (j = 1:1)
    d(1:4) = a(7:1:-2)
end do
do j = 1, 4
    if (abs(d(j) - real(9-2*j)) > 1.0e-6) error stop "reversed stride wrong"
end do

! reversed stride on an integer section
do concurrent (j = 1:1)
    ib(1:4) = ia(4:1:-1)
end do
do j = 1, 4
    if (ib(j) /= 5-j) error stop "integer reversed stride wrong"
end do

! strided section written into a strided target
b = 0.0
do concurrent (j = 1:1)
    b(1:7:2) = a(8:2:-2)
end do
do j = 1, 4
    if (abs(b(2*j-1) - real(10-2*j)) > 1.0e-6) error stop "strided target wrong"
    if (abs(b(2*j)) > 1.0e-6) error stop "strided target touched wrong element"
end do

! rank 2, reversed in the second dimension then in the first, as a
! negate-and-flip operator does
do concurrent (j = 1:1)
    q(1,1:4) = p(1,4:1:-1)
end do
do j = 1, 4
    if (abs(q(1,j) - real(10 + 5-j)) > 1.0e-6) error stop "row flip wrong"
end do

do concurrent (j = 1:1)
    q(1:4,2) = p(4:1:-1,2)
end do
do j = 1, 4
    if (abs(q(j,2) - real(10*(5-j) + 2)) > 1.0e-6) error stop "column flip wrong"
end do

! reversed section inside a larger expression
d = 0.0
do concurrent (j = 1:1)
    d(1:4) = -a(4:1:-1) * 2.0
end do
do j = 1, 4
    if (abs(d(j) + 2.0*real(5-j)) > 1.0e-6) error stop "reversed in expression wrong"
end do

! a plain contiguous section, which was never broken -- kept as a fence
d = 0.0
do concurrent (j = 1:1)
    d(1:4) = a(1:4)
end do
do j = 1, 4
    if (abs(d(j) - real(j)) > 1.0e-6) error stop "contiguous section wrong"
end do

print *, d
print *, b(1:7:2)
print *, ib
print *, q(1,:)
print *, q(:,2)
print *, "ok"
end program gpu_metal_230
