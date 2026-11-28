program param_array_zero_len_substr_01
  implicit none
  character(3), parameter :: a(4) = ' '
  character(*), parameter :: x(*) = a(2:2)(3:1)
  character(*), parameter :: y(*) = a(2:1)(3:1)
  if (size(x) /= 1 .or. len(x) /= 0) error stop 1
  if (size(y) /= 0 .or. len(y) /= 0) error stop 2
end program
