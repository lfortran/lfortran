program openmp_05
use omp_lib
integer :: n = 10, i

call omp_set_num_threads(4)

!$omp parallel private(i) shared(n)
!$omp do
do i = 1, n
    print *, "xyz"
    print *, "i = ", i, "from thread = ", omp_get_thread_num()
    if (omp_get_thread_num() > 4) error stop
end do
!$omp end do
!$omp end parallel

print *, "end program"

end program
