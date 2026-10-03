program gpu_metal_187
! erf() called through a function invoked from do concurrent (GPU offload)
implicit none
integer, parameter :: n = 64
real :: x(n), y(n), y_expected(n)
integer :: i

do i = 1, n
    x(i) = real(i - 1) * 0.05 - 1.5
    y_expected(i) = gelu_approx(x(i))
end do

do concurrent (i = 1:n)
    y(i) = gelu_approx(x(i))
end do

do i = 1, n
    if (abs(y(i) - y_expected(i)) > 1.0e-5) error stop
end do

print *, "PASSED"
contains
    pure function gelu_approx(x) result(y)
        real, intent(in) :: x
        real :: y
        y = 0.5 * x * (1.0 + erf(x / sqrt(2.0)))
    end function
end program
