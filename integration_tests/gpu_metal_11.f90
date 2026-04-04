program gpu_metal_11
! 2D do concurrent: matrix addition
implicit none
integer, parameter :: m = 128, n = 128
real :: a(m, n), b(m, n), c(m, n), c_expected(m, n)
integer :: i, j

do j = 1, n
    do i = 1, m
        a(i, j) = real(i + j)
        b(i, j) = real(i * j)
        c(i, j) = 0.0
        c_expected(i, j) = real(i + j) + real(i * j)
    end do
end do

do concurrent (i = 1:m, j = 1:n)
    c(i, j) = a(i, j) + b(i, j)
end do

do j = 1, n
    do i = 1, m
        if (abs(c(i, j) - c_expected(i, j)) > 1.0e-5) error stop
    end do
end do

print *, "PASSED"
end program
