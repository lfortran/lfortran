program gpu_metal_22
! 2D matrix element-wise multiply (Hadamard product)
implicit none
integer, parameter :: m = 100, n = 100
real :: a(m, n), b(m, n), c(m, n), c_expected(m, n)
integer :: i, j

do j = 1, n
    do i = 1, m
        a(i, j) = real(i) * 0.1
        b(i, j) = real(j) * 0.2
        c(i, j) = 0.0
        c_expected(i, j) = real(i) * 0.1 * real(j) * 0.2
    end do
end do

do concurrent (i = 1:m, j = 1:n)
    c(i, j) = a(i, j) * b(i, j)
end do

do j = 1, n
    do i = 1, m
        if (abs(c(i, j) - c_expected(i, j)) > 1.0e-4) error stop
    end do
end do

print *, "PASSED"
end program
