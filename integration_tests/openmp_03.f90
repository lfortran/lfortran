subroutine omp_func(n)
implicit none
integer :: i, n

!$omp parallel shared(i, n)
!$omp do
do i = 1, n
end do
!$omp end do
!$omp end parallel

print *, "n = ", n
if (n /= 100) error stop
end subroutine

program openmp_03
integer, parameter :: n = 100
call omp_func(n)
print *, "Done for n = ", n
end program
