program gpu_kernel_source_04
! Maths intrinsics on reals, which must not reach the backend untyped and
! bind to the integer overloads.
implicit none
integer, parameter :: n = 8
real :: x(n), y(n), y_expected(n)
integer :: i

do i = 1, n
    x(i) = -0.25 * real(i)
    y(i) = 0.0
    y_expected(i) = sqrt(abs(x(i))) + exp(x(i))
end do

do concurrent (i = 1:n)
    y(i) = sqrt(abs(x(i))) + exp(x(i))
end do

do i = 1, n
    if (abs(y(i) - y_expected(i)) > 1e-5) error stop
end do

print *, "PASSED"
end program
