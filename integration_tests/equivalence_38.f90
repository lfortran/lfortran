program equivalence_38
  implicit none

  character(4) :: source(1) = ["0123"]
  character(4) :: unit(1)
  character(4) :: value

  equivalence (unit, source)

  read(unit, "(a4)") value
  if (value /= "0123") error stop
end program equivalence_38
