program openmp_105
  ! A procedure contained in the program and called in a parallel region is
  ! copied into a module of its own, including a body that calls `reshape`.
  implicit none
  real :: s
  integer :: i
  s = 0
  !$omp parallel do reduction(+:s)
  do i = 1, 3
    s = s + offset_sum(i)
  end do
  !$omp end parallel do
  print *, s
  if (abs(s - 78) > 1e-5) error stop
contains
  function offset_sum(k) result(r)
    integer, intent(in) :: k
    real :: r
    real :: c(3, 2)
    c = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [3, 2])
    r = c(k, 2) + sum(c)
  end function
end program
