program gpu_kernel_source_07
! real(8) reaches the CUDA backend; only Metal has no double. The constant
! also pins the literal precision, which six significant figures would miss.
implicit none
integer, parameter :: n = 8
double precision, parameter :: c = 0.28209479177387814d0
double precision :: x(n), y(n), y_expected(n)
integer :: i

do i = 1, n
    x(i) = dble(i)
    y(i) = 0.0d0
    y_expected(i) = c * x(i) + 1.0d0/3.0d0
end do

do concurrent (i = 1:n)
    y(i) = c * x(i) + 1.0d0/3.0d0
end do

do i = 1, n
    if (abs(y(i) - y_expected(i)) > 1d-12) error stop
end do

print *, "PASSED"
end program
