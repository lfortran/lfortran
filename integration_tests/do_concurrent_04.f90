! equivalent to openmp_07.f90

subroutine static_counter(n, ctr)
use omp_lib
implicit none
integer, intent(in) :: n
integer, intent(inout) :: ctr

integer :: local_ctr

integer :: i

local_ctr = 1

do concurrent (i=1:n) reduce(*:local_ctr)
    local_ctr = local_ctr * 1
end do

ctr = ctr + local_ctr
end subroutine

program do_concurrent_04
use omp_lib
integer, parameter :: n = 1000000
integer :: ctr

call omp_set_num_threads(8)
ctr = 0
call static_counter(n, ctr)

print *, ctr
if (ctr /= 1) error stop
end program
