! equivalent to openmp_15.f90

subroutine initialize_array(n, a)
use omp_lib
implicit none
integer, intent(in) :: n
real, dimension(n), intent(out) :: a

integer :: i

do concurrent (i=1:n)
  a(i) = 12.91
end do

print *, a(1), a(n)
end subroutine

subroutine parallel_sum(n, a, ctr)
use omp_lib
implicit none
integer, intent(in) :: n, ctr
real, dimension(n), intent(in) :: a

integer :: i
real :: sum

sum = 0.0

do concurrent (i=1:n) reduce(+:sum)
  sum = sum + a(i)
end do

! here we have precision error both with gfortran and lfortran 
! setting different number of threads leads to different results
! but in any case output of gfortran and lfortran is the same
print *, sum
if ( ctr == 1 ) then
if (abs(sum - 12933994.0) > 1e-7) error stop
else
! if (abs(sum - 25867988.0) > 1e-7) error stop ! lfortran always gives 2.58679600e+07
end if

end subroutine

subroutine reduce_array(n, a, b)
implicit none
integer, intent(in) :: n
real, dimension(n), intent(in) :: a
real, dimension(n) :: b

integer :: i

do concurrent (i=2:n)
  b(i) = a(i) + a(i-1)
end do

b(1) = a(1) * 2
end subroutine


program do_concurrent_09
use omp_lib
integer, parameter :: n = 1000000
real, dimension(n) :: a, b

call omp_set_num_threads(4)

call initialize_array(n, a)

print *, a(1), a(n)

call parallel_sum(n, a, 1)

call reduce_array(n, a, b)

call parallel_sum(n, b, 0)

end program
