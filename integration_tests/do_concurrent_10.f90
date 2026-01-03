! equivalent to openmp_16.f90

subroutine initialize_array(n, m, b)
implicit none
integer, intent(in) :: n, m
double precision, intent(out) :: b(n, m)

integer :: i

do concurrent (i=1:n) shared(b, n)
  b(i, :) = 12.9d0
end do

end subroutine

subroutine parallel_sum(n, m, b)
implicit none
integer, intent(in) :: n, m
double precision, intent(out) :: b(n, m)
double precision :: res

integer :: i

res = 0.0d0

do concurrent (i=1:n) reduce(+:res) shared(b, n)
  res = res + sum(b(i, :))
end do

print *, 'Sum = ', res
if (abs(res - 7455168.0000000438d0) > 1e-12) error stop
end subroutine


program do_concurrent_10
use omp_lib
integer, parameter :: n = 1920, m = 301
double precision, dimension(n, m) :: a

print *, a(2, 3)

call omp_set_num_threads(8)
call initialize_array(n, m, a)

print *, a(2, 3)

call parallel_sum(n, m, a)

end program
