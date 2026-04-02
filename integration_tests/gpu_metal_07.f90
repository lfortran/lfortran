program gpu_metal_07
! Element-wise math: sqrt and abs
implicit none
integer, parameter :: n = 10000
real :: x(n), y(n), y_expected(n)
integer :: i

do i = 1, n
    x(i) = real(i)
    y_expected(i) = sqrt(real(i)) + abs(real(i) - 5000.0)
end do

do concurrent (i = 1:n)
    y(i) = sqrt(x(i)) + abs(x(i) - 5000.0)
end do

do i = 1, n
    if (abs(y(i) - y_expected(i)) > 1.0e-3) error stop
end do

print *, "PASSED"
end program
