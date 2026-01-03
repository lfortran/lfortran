program openmp_53
  use omp_lib
  implicit none
  integer :: x
  integer, parameter:: N = 0

  !$omp parallel shared(x)
  x=N
  !$omp barrier
    !$omp critical
    x = x + 1
    !$omp end critical
  !$omp end parallel

  print *, "Final x:", x
  if (x /= omp_get_max_threads()) error stop "x is not equal to number of threads"
end program openmp_53