program openmp_52
  use omp_lib
  implicit none
  integer, parameter :: N = 100, init=0
  integer :: a(N), i, total
  a = 1  ! Initialize all elements to 1

  !$omp parallel shared(a, total) private(i)
    total = init  ! Initialize total to 0
    !$omp barrier
    
    !$omp do
        do i = 1, N
            !$omp critical
            total = total + a(i)
            !$omp end critical
        end do
    !$omp end do
  !$omp end parallel

  print *, "Total sum:", total
  if (total /= N) error stop "Incorrect sum"
end program openmp_52