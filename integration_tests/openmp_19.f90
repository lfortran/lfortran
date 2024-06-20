subroutine intiailise_array_kernel(n, a)
integer, intent(in) :: n
real, dimension(:), intent(inout) :: a
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

subroutine initialise_array(n, a)
integer, intent(in) :: n
real, dimension(:), intent(inout) :: a

interface
subroutine intiailise_array_kernel(n, a)
integer, intent(in) :: n
real, dimension(:), intent(inout) :: a
end subroutine
end interface

call intiailise_array_kernel(n, a)

print *, a(1), a(n), a(12), a(12841)
if (abs(a(1) - 12.91) > 1e-8) error stop
if (abs(a(n) - 12.91) > 1e-8) error stop
if (abs(a(12) - 12.91) > 1e-8) error stop
if (abs(a(12841) - 12.91) > 1e-8) error stop
end subroutine


program openmp_19
use omp_lib
integer, parameter :: n = 100000
integer :: i
real, pointer :: b(:)

interface
subroutine initialise_array(n, a)
integer, intent(in) :: n
real, dimension(:), intent(inout) :: a
end subroutine
end interface

allocate(b(n))

call omp_set_num_threads(4)

call initialise_array(n, b)

print *, b(1), b(n), b(12), b(12841)
if (abs(b(1) - 12.91) > 1e-8) error stop
if (abs(b(n) - 12.91) > 1e-8) error stop
if (abs(b(12) - 12.91) > 1e-8) error stop
if (abs(b(12841) - 12.91) > 1e-8) error stop
end program
