! equivalent to openmp_30.f90

subroutine matrix_multiplication(l, m, n)
use omp_lib
integer :: l, m, n, i, j, k
integer :: seed
double precision :: a(l, n), b(l, m), c(m, n)
double precision :: start_time, end_time

seed = 123456789

b = 121.124D0
c = 29124.012D0

do concurrent (j = 1:n) shared(a, b, c, l, m, n) local(i, k)
    do concurrent (i = 1:l)
        a(i,j) = 0.0D+00
        do concurrent (k = 1:m)
            a(i,j) = a(i,j) + b(i,k) * c(k,j)
        end do
    end do
end do

print *, "sum(a): ", sum(a)
if (abs(sum(a) - (440952103687207.56D0)) > 1D-12) error stop
end subroutine

program do_concurrent_12
call matrix_multiplication(500, 500, 500)
end program
