program gpu_kernel_source_03
! A loop nested inside the do concurrent body, and a named coefficient array.
implicit none
integer, parameter :: n = 16
integer, parameter :: nc = 5
real, parameter :: cs(nc) = [1.0, 0.5, 0.25, 0.125, 0.0625]
real :: x(n), y(n)
real :: b0, b1, b2, twox
integer :: i, j

do i = 1, n
    x(i) = 0.5
    y(i) = 0.0
end do

do concurrent (i = 1:n)
    b0 = 0.0
    b1 = 0.0
    b2 = 0.0
    twox = 2.0 * x(i)
    do j = 1, nc
        b2 = b1
        b1 = b0
        b0 = twox*b1 - b2 + cs(nc - j + 1)
    end do
    y(i) = 0.5 * (b0 - b2)
end do

do i = 1, n
    if (abs(y(i) - 0.46875) > 1e-5) error stop
end do

print *, "PASSED"
end program
