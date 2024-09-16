subroutine omp_func(n)
    use omp_lib
implicit none
integer, intent(in) :: n
integer :: i
call omp_set_num_threads(5)
!$omp parallel  private(i) shared(n)
!$omp do
do i = 1, n
    !omp critical
    print *,omp_get_thread_num()
    print *, "xyz"
end do
!$omp end do
!$omp end parallel

print *, "n = ", n
if (n /= 10) error stop
end subroutine

program openmp_34
integer, parameter :: n = 10
call omp_func(n)
print *, "Done for n = ", n
end program