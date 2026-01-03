subroutine initialize_array(n, a)
use omp_lib
implicit none
integer, intent(in) :: n
real, dimension(n), intent(out) :: a

integer :: i

!$omp parallel
!$omp do
do i = 1, n
  a(i) = 12.91
end do
!$omp end do
!$omp end parallel

print *, a(1), a(n)
end subroutine

subroutine parallel_sum(n, a)
use omp_lib
implicit none
integer, intent(in) :: n
real, dimension(n), intent(in) :: a

integer :: i
real :: sum

sum = 0.0

!$omp parallel do reduction(+:sum)
do i = 1, n
  sum = sum + a(i)
end do
!$omp end parallel do

print *, 'Sum = ', sum
! here we have precision error both with gfortran and lfortran 
! setting different number of threads leads to different results
! but in any case output of gfortran and lfortran is the same
if (abs(sum - 12905576.0) > 1e-8) error stop

end subroutine

program openmp_14
use omp_lib
integer, parameter :: n = 1000000
real, dimension(n) :: a

call omp_set_num_threads(8)

call initialize_array(n, a)

call parallel_sum(n, a)

print *, a(1), a(n)
end program
