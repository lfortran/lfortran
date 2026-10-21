program openmp_106
  ! A scalar pointer used in a parallel region enters the region associated
  ! with its target: as a loop bound, read in the body, written through and
  ! listed in a firstprivate clause.
  implicit none
  integer :: i
  integer, target :: n, m
  real(8), target :: r
  integer, pointer :: b
  real(8), pointer :: q
  real :: x(10)
  n = 3
  r = 1.5d0
  b => n
  q => r
  x = 0
  !$omp parallel do
  do i = 1, b
    x(i) = i
  end do
  !$omp end parallel do
  if (abs(sum(x) - 6) > 1e-6) error stop "loop bound"
  !$omp parallel do
  do i = 1, 10
    x(i) = b + q + 4
  end do
  !$omp end parallel do
  if (abs(sum(x) - 85) > 1e-4) error stop "body"
  !$omp parallel
  !$omp single
  b = 7
  !$omp end single
  !$omp end parallel
  if (n /= 7) error stop "write through"
  m = 0
  !$omp parallel do firstprivate(b) reduction(+:m)
  do i = 1, 10
    m = m + b
  end do
  !$omp end parallel do
  if (m /= 70) error stop "firstprivate"
  print *, sum(x), n, m
end program
