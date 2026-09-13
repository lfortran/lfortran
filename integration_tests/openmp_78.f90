! `parallel do collapse(2)` must partition both loop levels, not just the outer one.
program openmp_78
implicit none
integer, parameter :: n = 40, m = 30
real :: a(n,m)
integer :: i, j
a = 0.0
!$omp parallel do collapse(2)
do j = 1, m
    do i = 1, n
        a(i,j) = real(i + j)
    end do
end do
!$omp end parallel do
do j = 1, m
    do i = 1, n
        if (abs(a(i,j) - real(i + j)) > 1e-5) error stop
    end do
end do
print *, "PASSED"
end program
