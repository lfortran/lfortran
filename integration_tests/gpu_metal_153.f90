program gpu_metal_153
! erf() intrinsic inside do concurrent (GPU offload)
implicit none
integer, parameter :: n = 64
real :: x(n), y(n), y_expected(n)
integer :: i

do i = 1, n
    x(i) = real(i - 1) * 0.05 - 1.5
    y_expected(i) = erf(x(i))
end do

do concurrent (i = 1:n)
    y(i) = erf(x(i))
end do

do i = 1, n
    if (abs(y(i) - y_expected(i)) > 1.0e-5) error stop
end do

print *, "PASSED"
end program
