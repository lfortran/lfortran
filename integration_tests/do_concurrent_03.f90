! equivalent to openmp_06.f90

subroutine increment_ctr(n, ctr)
! use omp_lib
implicit none
integer, intent(in) :: n
integer, intent(inout) :: ctr

integer :: local_ctr

integer :: i

local_ctr = 0

do concurrent (i = 1:n) reduce(+:local_ctr)
    local_ctr = local_ctr + 1
end do

ctr = ctr + local_ctr
end subroutine

program do_concurrent_03
use omp_lib
integer, parameter :: n = 1000000
integer :: ctr

call omp_set_num_threads(8)
ctr = 0
call increment_ctr(n, ctr)

print *, ctr
if (ctr /= 1000000) error stop
end program
