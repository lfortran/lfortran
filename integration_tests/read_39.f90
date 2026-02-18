program read_39
  implicit none
  integer :: n, ios
  real :: r
  character(3) :: input(2) = ['BAD','666']
  character(5) :: float_str = '3.14 '

  read(input(1), *, iostat=ios) n
  if (ios <= 0) error stop "Test 1 failed: iostat should be positive for bad input"
  read(input(2), *, iostat=ios) n
  if (ios /= 0) error stop "Test 2 failed: iostat should be 0 for valid input"
  if (n /= 666) error stop "Test 2 failed: n should be 666"
  read(input(1), *, iostat=ios) r
  if (ios <= 0) error stop "Test 3 failed: iostat should be positive for bad real input"
  read(float_str, *, iostat=ios) r
  if (ios /= 0) error stop "Test 4 failed: iostat should be 0 for valid real input"
end program read_39
