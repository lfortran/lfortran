! Single-ranged-dim sections on rank-2 arrays for the C++ backend:
! column slices and partial-column updates lower to flat-index loops.
program arrays_141
implicit none
integer, parameter :: n = 4
real :: A(n, n), x(n)
real :: alpha
integer :: i, j
x = [1.0, 2.0, 3.0, 4.0]
A = 0.0
alpha = 2.0
j = 2
A(j:n, j) = A(j:n, j) + alpha*x(j:n)*x(j)
if (abs(A(2, 2) - 8.0) > 1e-6) error stop 1
if (abs(A(3, 2) - 12.0) > 1e-6) error stop 2
if (abs(A(4, 2) - 16.0) > 1e-6) error stop 3
if (abs(A(1, 2) - 0.0) > 1e-6) error stop 4
A = 0.0
A(:, j) = 1.0
do i = 1, n
    if (abs(A(i, j) - 1.0) > 1e-6) error stop 5
end do
if (abs(A(1, 1) - 0.0) > 1e-6) error stop 6
A(1:2, 1) = x(3:4)
if (abs(A(1, 1) - 3.0) > 1e-6) error stop 7
if (abs(A(2, 1) - 4.0) > 1e-6) error stop 8
print *, A(2, 2), A(1, 1)
end program arrays_141
