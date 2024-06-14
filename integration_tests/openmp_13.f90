subroutine initialize_array_kernel_1(n, a)
integer, intent(in) :: n
real, intent(out) :: a(n)
integer :: i
!$omp parallel shared(a) private(i)
!$omp do
do i = 1, n
  a(i) = 12.91
end do
!$omp end do
!$omp end parallel
print *, a(1), a(n), a(12), a(12841)
if (abs(a(1) - 12.91) > 1e-8) error stop
if (abs(a(n) - 12.91) > 1e-8) error stop
if (abs(a(12) - 12.91) > 1e-8) error stop
if (abs(a(12841) - 12.91) > 1e-8) error stop
end subroutine

subroutine initialize_array(n, a)
use omp_lib
implicit none
integer, intent(in) :: n
real, dimension(n), intent(out) :: a
integer :: i
call initialize_array_kernel_1(n, a)
print *, a(1), a(n), a(12), a(12841)
if (abs(a(1) - 12.91) > 1e-8) error stop
if (abs(a(n) - 12.91) > 1e-8) error stop
if (abs(a(12) - 12.91) > 1e-8) error stop
if (abs(a(12841) - 12.91) > 1e-8) error stop
end subroutine

program openmp_13
use omp_lib
integer, parameter :: n = 100000
real, dimension(n) :: a

print *, a(2)

call omp_set_num_threads(4)
call initialize_array(n, a)

print *, a(1), a(n), a(12), a(12841)
if (abs(a(1) - 12.91) > 1e-8) error stop
if (abs(a(n) - 12.91) > 1e-8) error stop
if (abs(a(12) - 12.91) > 1e-8) error stop
if (abs(a(12841) - 12.91) > 1e-8) error stop
end program
