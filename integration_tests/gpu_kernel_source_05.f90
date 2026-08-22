program gpu_kernel_source_05
! The power operator, which used to reach the backend as a literal **.
implicit none
integer, parameter :: n = 8
real :: x(n), y(n), y_expected(n)
integer :: i

do i = 1, n
    x(i) = real(i)
    y(i) = 0.0
    y_expected(i) = x(i)**2 + x(i)**0.5
end do

do concurrent (i = 1:n)
    y(i) = x(i)**2 + x(i)**0.5
end do

do i = 1, n
    if (abs(y(i) - y_expected(i)) > 1e-4) error stop
end do

print *, "PASSED"
end program
