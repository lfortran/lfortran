subroutine increment_ctr(n, ctr)
! use omp_lib
implicit none
integer, intent(in) :: n
integer, intent(inout) :: ctr

integer :: local_ctr

integer :: i

!$omp parallel private(i) reduction(+:local_ctr)
!$omp do
do i = 1, n
    local_ctr = local_ctr + 1
end do
!$omp end do
!$omp end parallel

ctr = ctr + local_ctr
end subroutine

program openmp1
integer, parameter :: n = 1000000
integer :: ctr

ctr = 0
call increment_ctr(n, ctr)

print *, ctr
end program
