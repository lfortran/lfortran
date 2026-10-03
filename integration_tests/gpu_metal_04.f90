program gpu_metal_04
! Double precision SAXPY
implicit none
integer, parameter :: n = 50000
double precision :: x(n), y(n), y_expected(n), a
integer :: i

a = 3.0d0
do i = 1, n
    x(i) = dble(i) * 0.1d0
    y(i) = dble(i) * 0.2d0
    y_expected(i) = a * dble(i) * 0.1d0 + dble(i) * 0.2d0
end do

do concurrent (i = 1:n)
    y(i) = a * x(i) + y(i)
end do

do i = 1, n
    if (abs(y(i) - y_expected(i)) > 1.0d-10) error stop
end do

print *, "PASSED"
end program
