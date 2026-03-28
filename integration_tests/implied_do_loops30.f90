program implied_do_loops30
  implicit none
  integer :: a(2,2), b(4), i
  integer :: c(2,3), d(6)
  a = reshape([1, 2, 3, 4], [2,2])
  b = [(a, i = 1, 1)]
  if (b(1) /= 1) error stop
  if (b(2) /= 2) error stop
  if (b(3) /= 3) error stop
  if (b(4) /= 4) error stop

  c = reshape([10, 20, 30, 40, 50, 60], [2,3])
  d = [(c, i = 1, 1)]
  if (d(1) /= 10) error stop
  if (d(2) /= 20) error stop
  if (d(3) /= 30) error stop
  if (d(4) /= 40) error stop
  if (d(5) /= 50) error stop
  if (d(6) /= 60) error stop

  print *, "PASS"
end program
