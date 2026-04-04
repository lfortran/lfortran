program gpu_metal_16
! 2D do concurrent with non-parameter bounds
implicit none
integer :: m, n
real :: a(200, 200), a_expected(200, 200)
integer :: i, j

m = 150
n = 100
do j = 1, 200
    do i = 1, 200
        a(i, j) = 0.0
        a_expected(i, j) = 0.0
    end do
end do
do j = 1, n
    do i = 1, m
        a_expected(i, j) = real(i) * real(j)
    end do
end do

do concurrent (i = 1:m, j = 1:n)
    a(i, j) = real(i) * real(j)
end do

do j = 1, 200
    do i = 1, 200
        if (abs(a(i, j) - a_expected(i, j)) > 1.0e-5) error stop
    end do
end do

print *, "PASSED"
end program
