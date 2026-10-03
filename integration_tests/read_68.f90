program read_68
  implicit none
  integer, parameter :: dp = kind(1.0d0)
  character(len=50) :: line
  real(dp) :: a, b, c
  integer :: ios, x, y

  ! Test 1: comma-separated reals with spaces after comma
  line = "1.0, 2.0"
  read(line, *, iostat=ios) a, b
  if (ios /= 0) error stop
  if (abs(a - 1.0_dp) > 1.0e-10_dp) error stop
  if (abs(b - 2.0_dp) > 1.0e-10_dp) error stop

  ! Test 2: comma-separated reals without spaces
  line = "3.5,4.5"
  read(line, *, iostat=ios) a, b
  if (ios /= 0) error stop
  if (abs(a - 3.5_dp) > 1.0e-10_dp) error stop
  if (abs(b - 4.5_dp) > 1.0e-10_dp) error stop

  ! Test 3: multiple spaces after comma
  line = "5.0,   6.0,   7.0"
  read(line, *, iostat=ios) a, b, c
  if (ios /= 0) error stop
  if (abs(a - 5.0_dp) > 1.0e-10_dp) error stop
  if (abs(b - 6.0_dp) > 1.0e-10_dp) error stop
  if (abs(c - 7.0_dp) > 1.0e-10_dp) error stop

  ! Test 4: comma-separated integers with spaces
  line = "10, 20"
  read(line, *, iostat=ios) x, y
  if (ios /= 0) error stop
  if (x /= 10) error stop
  if (y /= 20) error stop

  print *, "ok"
end program
