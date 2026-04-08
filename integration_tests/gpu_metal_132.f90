! Test: sum() nested inside a larger expression in do concurrent.
! Previously generated invalid Metal shader because sum() inside an
! expression like "results(i) = bias + sum(a)" was not inlined,
! producing a call to _lcompilers_Sum which is unavailable in Metal.
program gpu_metal_132
  implicit none
  integer :: i
  real :: a(3), results(4), bias
  a = 0.5
  bias = 1.0
  do concurrent (i = 1:4)
    results(i) = bias + sum(a)
  end do
  do i = 1, 4
    if (abs(results(i) - 2.5) > 1.0e-5) error stop
  end do
  print *, "PASS"
end program
