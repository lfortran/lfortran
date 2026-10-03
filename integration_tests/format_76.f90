program format_76
  implicit none

  real :: a
  character(9) :: a_string

  a_string = '  987.654'
  read (a_string, '(0PF9.4)') a

  if (abs(a - 987.654) >= 0.001) error stop
end program format_76