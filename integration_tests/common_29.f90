program common_29
  common /block/ t
  integer :: t
  integer :: i, s

  t = 5
  if (t /= 5) error stop
  s = 0
  do, i = 1, t
    s = s + i
  end do
  if (s /= 15) error stop
end program
