subroutine initialise_array(n)
integer, intent(in) :: n
real, dimension(n) :: b
integer :: i

!$omp parallel shared(b) private(i)
!$omp do
do i = 1, n
  b(i) = 12.91
end do
!$omp end do
!$omp end parallel

print *, b(1), b(n)
print *, b(1), b(n), b(12), b(12841)
if (abs(b(1) - 12.91) > 1e-8) error stop
if (abs(b(n) - 12.91) > 1e-8) error stop
if (abs(b(12) - 12.91) > 1e-8) error stop
if (abs(b(12841) - 12.91) > 1e-8) error stop

end subroutine

program openmp_20
use omp_lib
integer, parameter :: n = 100000
call omp_set_num_threads(4)
call initialise_array(n)
end program
