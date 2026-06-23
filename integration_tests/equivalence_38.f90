program equivalence_38
  implicit none

  character(1), dimension(1) :: abuf = ["x"]
  character(1), dimension(1) :: buf
  character(1) :: a

  equivalence (buf, abuf)

  read(buf, "(a1)") a
  if (a /= "x") error stop
end program
