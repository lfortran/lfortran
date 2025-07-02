program openmp_58
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
  !$omp taskloop shared(A)
  do i = 1, N
        total = total + A(index) * 2
        index=index+1
    end do
  !$omp end taskloop
  !$omp end single
  !$omp taskwait
  !$omp end parallel

  print *, "Total = ", total, index
  if(total/=10) error stop
end program openmp_58