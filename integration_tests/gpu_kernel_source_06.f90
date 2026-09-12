program gpu_kernel_source_06
! Rank-2 subscripts, which the kernel has to flatten column-major by hand.
implicit none
integer, parameter :: m = 4
integer, parameter :: nc = 5
real :: a(m,nc), b(m,nc)
integer :: i, j

do j = 1, nc
    do i = 1, m
        a(i,j) = real(i) + 10.0 * real(j)
        b(i,j) = 0.0
    end do
end do

do concurrent (i = 1:m, j = 1:nc)
    b(i,j) = 2.0 * a(i,j)
end do

do j = 1, nc
    do i = 1, m
        if (abs(b(i,j) - 2.0 * a(i,j)) > 1e-3) error stop
    end do
end do

print *, "PASSED"
end program
