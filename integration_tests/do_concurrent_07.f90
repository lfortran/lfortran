! equivalent to openmp_12.f90

subroutine initialize_array(n, b)
use omp_lib
implicit none
integer, intent(in) :: n
real, dimension(n), intent(out) :: b

integer :: i

do concurrent (i=1:n) shared(b) local(i)
  b(i) = 12.91
end do


print *, b(1), b(n)
if (abs(b(1) - 12.91) > 1e-8) error stop
if (abs(b(n) - 12.91) > 1e-8) error stop
if (abs(b(12) - 12.91) > 1e-8) error stop
if (abs(b(12841) - 12.91) > 1e-8) error stop
end subroutine

program do_concurrent_07
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
