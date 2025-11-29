program assigned_fmt_read
  implicit none
  integer :: i

  assign 13 to i
13 format ()
  read (5, i)

end program
