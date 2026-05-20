program format_87
  implicit none

  character(32) :: cdata(3)

  write (cdata, 100)
100 format(tr6, " 123.40 567.80", t5, "  12.34506.78", &
             1x, "120.34 506.78" // &
           tr6, " 123.40 567.80", t5, " +12.34506.78", &
             1x, "120.34 506.78")

  if (cdata(1) /= '      12.34506.78.120.34 506.78') error stop
  if (cdata(2) /= ' ') error stop
  if (cdata(3) /= '     +12.34506.78.120.34 506.78') error stop

end program format_87
