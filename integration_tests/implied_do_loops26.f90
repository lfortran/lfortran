program implied_do_loops26
  implicit none

  integer :: idata(11)
  integer :: i

! AC implied-do variable scope test

  i = -42
  idata = [(i, i = 1, size(idata))]

  if (i /= -42) error stop

! IO implied-do behaviour

  i = -42
  write (*, *) (idata(i), i = 1, size(idata))
  if (i /= size(idata) + 1) error stop

  i = -42
  write (*, *) (idata(i), i = 1, size(idata), 2)
  if (i /= size(idata) + 2) error stop

  write (*, *) (idata(i), i = i - 2, 1, -2)
  if (i /= -1) error stop

  print *, "All tests passed."
end program implied_do_loops26
