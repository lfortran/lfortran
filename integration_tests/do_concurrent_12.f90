! equivalent to openmp_30.f90

subroutine matrix_multiplication(l, m, n)
use omp_lib
integer, parameter :: dp = kind(0.d0)
integer :: l, m, n, i, j, k
integer :: seed
real(dp) :: a(l, n), b(l, m), c(m, n)
real(dp) :: start_time, end_time

seed = 123456789

b = 121.124D0
c = 29124.012D0

do concurrent (j = 1:n) shared(a, b, c, l, m, n) local(i, k)
    do concurrent (i = 1:l)
        a(i,j) = 0
        do concurrent (k = 1:m)
            a(i,j) = a(i,j) + b(i,k) * c(k,j)
        end do
    end do
end do

print *, "sum(a): ", sum(a)
if (abs(sum(a) - (4.40952103686020386e+11_dp)) > 1e-12_dp) error stop
end subroutine

program do_concurrent_12
call matrix_multiplication(50, 50, 50)
end program
