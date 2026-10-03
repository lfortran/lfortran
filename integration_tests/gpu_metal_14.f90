program gpu_metal_14
! Multiple arrays combined: z = alpha*x + beta*y
implicit none
integer, parameter :: n = 50000
real :: x(n), y(n), z(n), z_expected(n)
real :: alpha, beta
integer :: i

alpha = 2.5
beta = -1.5
do i = 1, n
    x(i) = real(i) * 0.1
    y(i) = real(n - i + 1) * 0.1
    z(i) = 0.0
    z_expected(i) = alpha * real(i) * 0.1 + beta * real(n - i + 1) * 0.1
end do

do concurrent (i = 1:n)
    z(i) = alpha * x(i) + beta * y(i)
end do

do i = 1, n
    if (abs(z(i) - z_expected(i)) > 1.0e-3) error stop
end do

print *, "PASSED"
end program
