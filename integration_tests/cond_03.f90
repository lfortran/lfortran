
program cond_03
  implicit none

  real, pointer :: d1, d2
  real, target :: t1, t2

  ! Initialize target objects
  t1 = 4
  t2 = 5
  ! Assign pointers
  d1 => t1
  d2 => t2

  if ( d1 > d2) error stop "Test_1 failed"

  t1 = 7
  t2 = 6

  if ( d1 < d2) error stop "Test_2 failed"

end program cond_03