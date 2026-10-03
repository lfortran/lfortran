program read_57
  ! Test list-directed internal read with multiple values of mixed types
  implicit none
  character(len=20) :: line
  integer :: x, y
  character(1) :: c
  real :: r
  character(len=5) :: s

  ! Integer then character
  line = '1 A'
  read(line, *) x, c
  if (x /= 1) error stop
  if (c /= 'A') error stop

  ! Two integers
  line = '10 20'
  read(line, *) x, y
  if (x /= 10) error stop
  if (y /= 20) error stop

  ! Integer then real
  line = '3 2.5'
  read(line, *) x, r
  if (x /= 3) error stop
  if (abs(r - 2.5) > 1.0e-6) error stop

  ! Comma-separated values
  line = '7,8'
  read(line, *) x, y
  if (x /= 7) error stop
  if (y /= 8) error stop

  ! Three values: integer, real, character
  line = '42 3.14 Z'
  read(line, *) x, r, c
  if (x /= 42) error stop
  if (abs(r - 3.14) > 0.01) error stop
  if (c /= 'Z') error stop

  ! Single value (regression: should still work)
  line = '99'
  read(line, *) x
  if (x /= 99) error stop

  print *, 'PASS'
end program
