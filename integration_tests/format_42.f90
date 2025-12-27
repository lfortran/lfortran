program format_42
  implicit none
  real :: val1, val2
  character(len=50) :: str1, str2

  val1 = 42.43
  val2 = 123.456

  ! Test PE format with exponent width (E_int suffix)
  write(str1, 100) val1
  write(str2, 200) val2

  write(*, 100) val1
  write(*, 200) val2

  ! Verify the format works correctly
  ! With 3P scale factor: 42.43 displays as 424.3000E-01
  ! The format is E14.6E2, so it uses 14 characters total
  if (trim(adjustl(str1)) /= '424.3000E-01') error stop "PE14.6E2 format failed"

  ! With 2P scale factor: 123.456 displays as 12.3456E+01
  ! The format is E12.5E2, so it uses 12 characters total
  if (trim(adjustl(str2)) /= '12.3456E+01') error stop "PE12.5E2 format failed"

  print *, "PASSED"

100 format (3PE14.6E2)
200 format (2PE12.5E2)

end program
