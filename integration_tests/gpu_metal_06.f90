program gpu_metal_06
! Non-parameter (runtime) loop bounds
implicit none
integer :: n
real :: x(1000), y(1000), y_expected(1000)
integer :: i

n = 500
do i = 1, 1000
    x(i) = real(i)
    y(i) = 0.0
    y_expected(i) = 0.0
end do
do i = 1, n
    y_expected(i) = x(i) * 2.0
end do

do concurrent (i = 1:n)
    y(i) = x(i) * 2.0
end do

do i = 1, 1000
    if (abs(y(i) - y_expected(i)) > 1.0e-5) error stop
end do

print *, "PASSED"
end program
