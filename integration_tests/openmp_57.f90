program openmp_57
  use omp_lib
  implicit none
  integer, parameter :: N = 5
  integer :: A(N)
  integer :: i, index,total

  A = 1
  total=0
  index=1

  !$omp parallel
  !$omp single
  do i = 1, N
    !$omp task shared(A)
        total = total + A(index) * 2
        index=index+1
    !$omp end task
  end do
  !$omp end single
  !$omp taskwait
  !$omp end parallel

  print *, "Total = ", total, index
  if(total/=10) error stop
end program openmp_57