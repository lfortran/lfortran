program format_before_read_01
  implicit none
  integer :: i, j, k
  character(len=7) :: input_string

  ! Test case: FORMAT statement precedes the READ statement
  ! This tests that the compiler correctly handles forward references
  ! to FORMAT statements

  input_string = "123 45 "

100 format (I3, I2, I2)
  read(input_string, 100) i, j, k

  if (i /= 123) error stop "Expected i = 123"
  if (j /= 4) error stop "Expected j = 4"
  if (k /= 5) error stop "Expected k = 5"

  print *, "Test passed: FORMAT before READ works correctly"

end program format_before_read_01
