subroutine increment_ctr(n, ctr)
use omp_lib
implicit none
integer, intent(in) :: n
integer, intent(inout) :: ctr

integer :: local_ctr

integer :: i

local_ctr = 0
!$omp parallel private(i) reduction(+:local_ctr)
!$omp do
do i = 1, n
    local_ctr = local_ctr + 1
end do
!$omp end do
! the following condition shall be true almost always
! if (local_ctr > ( n / omp_get_max_threads()) + 5) error stop ! TODO: fix this, it was previously wrong in LFortran.
! With this PR, we get correct ASR and thus error stop is triggered, previously the ASR node was before
! the parallel region and thus the error stop was not triggered.
! To fix this we need to parallelize the if statement, which is not supported yet.
! Refer: https://github.com/lfortran/lfortran/issues/4147 to fix it
!$omp end parallel

ctr = ctr + local_ctr
end subroutine

program openmp_06
use omp_lib
integer, parameter :: n = 1000000
integer :: ctr

call omp_set_num_threads(8)
ctr = 0
call increment_ctr(n, ctr)

print *, ctr
if (ctr /= 1000000) error stop
end program
