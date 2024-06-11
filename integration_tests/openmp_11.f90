subroutine increment_ctr(n, ctr)
use omp_lib
implicit none
integer, intent(in) :: n
complex, intent(inout) :: ctr

complex :: local_ctr

integer :: i

local_ctr = (0,0)
!$omp parallel private(i) reduction(+:local_ctr)
!$omp do
do i = 1, n
    local_ctr = local_ctr + i
end do
!$omp end do
!$omp end parallel

ctr = ctr + local_ctr
end subroutine

program openmp_06
use omp_lib
integer, parameter :: n = 1000
complex :: ctr
complex :: res = (0,0)
res = ((n * (n+1))/2) ! Sum from 1 -> 1000
call omp_set_num_threads(8)
ctr = (0,0)
call increment_ctr(n, ctr)
print *, ctr
if(ctr /= res) error stop
end program