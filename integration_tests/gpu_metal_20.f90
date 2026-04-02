program gpu_metal_20
! Multiple statements per iteration: normalize and shift
implicit none
integer, parameter :: n = 10000
real :: x(n), y(n), y_expected(n)
real :: norm, shift
integer :: i

norm = 100.0
shift = 5.0
do i = 1, n
    x(i) = real(i)
    y_expected(i) = real(i) / norm + shift
end do

do concurrent (i = 1:n)
    y(i) = x(i) / norm
    y(i) = y(i) + shift
end do

do i = 1, n
    if (abs(y(i) - y_expected(i)) > 1.0e-5) error stop
end do

print *, "PASSED"
end program
