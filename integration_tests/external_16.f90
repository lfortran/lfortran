character(8) function sin()
  sin = 'Peccavi!'
end function sin

program external_16
  implicit none
  character(8), external :: sin
  character(8) :: res
  res = sin()
  if (res /= 'Peccavi!') error stop
  print '(A)', res
end program external_16
