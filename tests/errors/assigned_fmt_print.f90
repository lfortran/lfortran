program assigned_fmt_print
  implicit none
  integer :: i

  assign 100 to i
100 format (A)
  print i, "test"

end program
