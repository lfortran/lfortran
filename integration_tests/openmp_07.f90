subroutine static_counter(n, ctr)
use omp_lib
implicit none
integer, intent(in) :: n
integer, intent(inout) :: ctr

integer :: local_ctr

integer :: i

local_ctr = 1
!$omp parallel private(i) reduction(*:local_ctr)
!$omp do
do i = 1, n
    local_ctr = local_ctr * 1
end do
!$omp end do
! the following condition shall be true almost always
if (local_ctr > ( n / omp_get_max_threads()) + 5) error stop
!$omp end parallel

ctr = ctr + local_ctr
end subroutine

program openmp_07
use omp_lib
integer, parameter :: n = 1000000
integer :: ctr

call omp_set_num_threads(8)
ctr = 0
call static_counter(n, ctr)

print *, ctr
if (ctr /= 1) error stop
end program
