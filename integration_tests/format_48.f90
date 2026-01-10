program mre_read_string_fmt
  implicit none

  character(len=3) :: local_chars
  integer :: basevalue

  local_chars = "1 0"
  read(local_chars, "(bn,g3.0)") basevalue

  if (basevalue /= 10) error stop "wrong value"

  print *, "test passed"
end program mre_read_string_fmt
