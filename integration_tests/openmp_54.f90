program openmp_54
  use omp_lib
  implicit none

  integer, parameter :: N = 1000
  integer :: i, tid
  integer :: total_sum
  integer :: partial_sum


  !$omp parallel shared(total_sum) private(i, partial_sum, tid)
    tid = omp_get_thread_num()
    partial_sum = 0
    total_sum = 0
    !$omp barrier

    !$omp do
        do i = 1, N
            partial_sum = partial_sum + i
        end do
    !$omp end do

    ! Critical update to the shared total_sum
    !$omp critical
        total_sum = total_sum + partial_sum
        print *, "Thread ", tid, " added partial_sum ", partial_sum
    !$omp end critical

    !$omp barrier

    !$omp single
        if (total_sum /= N*(N+1)/2) then
            print *, "ERROR: total_sum = ", total_sum, " expected = ", N*(N+1)/2
            error stop
        else
            print *, "Success! total_sum = ", total_sum
        end if
    !$omp end single

  !$omp end parallel

end program openmp_54