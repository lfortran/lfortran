program equivalence_37
  implicit none

  character(len=4) :: x(8) = (/ &
      "abcd", "efgh", "ijkl", "mnop", &
      "qrst", "uvwx", "yz01", "2345" /)
  character(len=4) :: y(2, 4)
  character(len=8) :: z

  equivalence (x, y)

  read(y, "(a8)") z
  if (z /= "abcd    ") error stop
end program
