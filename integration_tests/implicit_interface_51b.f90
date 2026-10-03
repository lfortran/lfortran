subroutine check(actual, expected)
  implicit none
  character(len=*) :: actual
  character(len=*) :: expected
  print *, "'", actual, "' len =", len(actual)
  if (len(actual) /= len(expected)) error stop
  if (actual /= expected) error stop
end subroutine check

subroutine check_with_array(scalar, arr, expected_scalar)
  implicit none
  character(len=*) :: scalar, expected_scalar
  character(len=*) :: arr(*)
  print *, "'", scalar, "' len =", len(scalar)
  if (len(scalar) /= len(expected_scalar)) error stop
  if (scalar /= expected_scalar) error stop
  if (len(arr) < 1) error stop
end subroutine check_with_array
