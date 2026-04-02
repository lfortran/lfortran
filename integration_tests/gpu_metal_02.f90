program gpu_metal_02
! 2D do concurrent: matrix scaling
implicit none
integer, parameter :: m = 256, n = 128
real :: a(m, n), a_expected(m, n), scale
integer :: i, j

scale = 3.0
do j = 1, n
    do i = 1, m
        a(i, j) = real(i + j)
        a_expected(i, j) = real(i + j) * scale
    end do
end do

do concurrent (i = 1:m, j = 1:n)
    a(i, j) = a(i, j) * scale
end do

do j = 1, n
    do i = 1, m
        if (abs(a(i, j) - a_expected(i, j)) > 1.0e-5) error stop
    end do
end do

print *, "PASSED"
end program
