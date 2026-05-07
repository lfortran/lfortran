program doloop_20
  implicit none
  real :: civ
  do, civ = 1.1, 2.4, 0.5
    print *, civ
  end do

  if (abs(civ - 2.6) >= 0.001) error stop "incorrect final value"
  print *, "Test passed"
end program doloop_20