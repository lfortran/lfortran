program gpu_metal_10
! Local scalar temporaries inside do concurrent
implicit none
integer, parameter :: n = 10000
real :: x(n), y(n), y_expected(n)
integer :: i
real :: s, c

do i = 1, n
    x(i) = real(i) * 0.01
    y_expected(i) = sin(real(i) * 0.01) + cos(real(i) * 0.01)
end do

do concurrent (i = 1:n)
    s = sin(x(i))
    c = cos(x(i))
    y(i) = s + c
end do

do i = 1, n
    if (abs(y(i) - y_expected(i)) > 1.0e-4) error stop
end do

print *, "PASSED"
end program
