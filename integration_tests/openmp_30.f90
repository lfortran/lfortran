subroutine matrix_multiplication(l, m, n)
use omp_lib
integer :: l, m, n, i, j, k
integer :: seed
double precision :: a(l, n), b(l, m), c(m, n)
double precision :: start_time, end_time

seed = 123456789

b = 121.124D0
c = 29124.012D0

start_time = omp_get_wtime()
!$omp parallel do shared(a, b, c, l, m, n) private(i, j, k)
do j = 1, n
    do i = 1, l
        a(i,j) = 0.0D+00
        do k = 1, m
            a(i,j) = a(i,j) + b(i,k) * c(k,j)
        end do
    end do
end do
!$omp end parallel do
end_time = omp_get_wtime()

print *, "Time: ", end_time - start_time
print *, "sum(a): ", sum(a)
! if (abs(sum(a) - (440952103687207.56D0)) > 1D-12) error stop
end subroutine

program openmp_30
call matrix_multiplication(500, 500, 500)
end program
