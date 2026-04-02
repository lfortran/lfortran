program gpu_metal_09
! Non-unit stride via index arithmetic
implicit none
integer, parameter :: n = 5000
real :: a(n), a_expected(n)
integer :: i

do i = 1, n
    a(i) = 0.0
    a_expected(i) = 0.0
end do
do i = 1, n / 2
    a_expected(2 * i) = real(i)
end do

do concurrent (i = 1:n/2)
    a(2 * i) = real(i)
end do

do i = 1, n
    if (abs(a(i) - a_expected(i)) > 1.0e-5) error stop
end do

print *, "PASSED"
end program
