program equivalence_22
  implicit none

  character(43) :: ca_2(2)
  character(1) :: ca_full(97)
  equivalence (ca_2, ca_full)

  ca_full = 'x'
  ca_2(1) = '1'
  ca_2(2) = '2'

  if (ca_full(1) /= '1') error stop
  if (ca_full(2) /= ' ') error stop
  if (ca_full(43) /= ' ') error stop
  if (ca_full(44) /= '2') error stop
  if (ca_full(45) /= ' ') error stop
  if (ca_full(86) /= ' ') error stop
  if (ca_full(87) /= 'x') error stop
  if (ca_full(97) /= 'x') error stop

end program
