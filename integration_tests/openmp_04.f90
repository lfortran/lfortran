subroutine omp_func(n)
use omp_lib
implicit none
integer, intent(in) :: n
integer :: i

!$omp parallel private(i) shared(n)
!$omp do
do i = 1, n
    print *, "xyz"
    print *, "i = ", i, "from thread = ", omp_get_thread_num()
    if (omp_get_thread_num() > 4) error stop
end do
!$omp end do
!$omp end parallel

print *, "n = ", n
if (n /= 10) error stop
end subroutine

program openmp_04
use omp_lib
integer :: n = 10

call omp_set_num_threads(4)
call omp_func(n)
print *, "Done for n = ", n

end program
