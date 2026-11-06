! The kernel of a reduction sets every accumulator before it reads it, so
! the launch hands them to the device write-discard: nothing is copied in.
! Found in the dot product of POT3D's conjugate gradient (cgdot).
program gpu_reduction_overwrite_01
implicit none
integer, parameter :: n = 1000000
real(4), allocatable :: x(:), y(:)
real(4) :: s, m
integer :: i, it, k
allocate(x(n), y(n))
x = 1
y = 2
s = 0
do it = 1, 3
    k = n
    if (it == 2) k = 10
    do concurrent (i = 1:k) reduce(+:s)
        s = s + x(i)*y(i)
    end do
end do
if (abs(s - 4000020.0) > 0.5) error stop
s = 0
m = -huge(m)
do concurrent (i = 1:n) reduce(+:s) reduce(max:m)
    s = s + x(i)*y(i)
    m = max(m, x(i)*y(i))
end do
if (abs(s - 2e6) > 0.5) error stop
if (abs(m - 2) > 1e-6) error stop
print *, s, m
end program
