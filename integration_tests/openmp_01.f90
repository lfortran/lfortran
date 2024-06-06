subroutine omp_func(n)
implicit none
integer, intent(in) :: n
integer :: i

!$omp parallel private(i) shared(n)
!$omp do
do i = 1, n
    ! print *, "xyz"
end do
!$omp end do
!$omp end parallel

print *, "n = ", n
if (n /= 100) error stop
end subroutine

program openmp_01
integer, parameter :: n = 100
call omp_func(n)
print *, "Done for n = ", n
end program
