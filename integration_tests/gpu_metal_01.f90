program gpu_metal_01
implicit none
integer, parameter :: n = 1000
real :: x(n), y(n), y_expected(n), a
integer :: i

a = 2.0
do i = 1, n
    x(i) = real(i)
    y(i) = real(i) * 0.5
    y_expected(i) = a * real(i) + real(i) * 0.5
end do

do concurrent (i = 1:n)
    y(i) = a * x(i) + y(i)
end do

do i = 1, n
    if (abs(y(i) - y_expected(i)) > 1.0e-5) error stop
end do

print *, "PASSED"
end program
