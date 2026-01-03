subroutine initialize_array(n, m, b)
implicit none
integer, intent(in) :: n, m
double precision, intent(out) :: b(n, m)

integer :: i

!$omp parallel shared(b, n) private(i)
!$omp do
do i = 1, n
  b(i, :) = 12.9d0
end do
!$omp end do
!$omp end parallel

end subroutine

subroutine parallel_sum(n, m, b)
implicit none
integer, intent(in) :: n, m
double precision, intent(out) :: b(n, m)
double precision :: res

integer :: i

res = 0.0d0 

!$omp parallel shared(b, n) private(i) reduction(+:res)
!$omp do
do i = 1, n
res = res + sum(b(i, :))
end do
!$omp end do
!$omp end parallel

print *, 'Sum = ', res
if (abs(res - 7455168.0000000438d0) > 1e-12) error stop
end subroutine


program openmp_16
use omp_lib
integer, parameter :: n = 1920, m = 301
double precision, dimension(n, m) :: a

print *, a(2, 3)

call omp_set_num_threads(8)
call initialize_array(n, m, a)

print *, a(2, 3)

call parallel_sum(n, m, a)

end program

