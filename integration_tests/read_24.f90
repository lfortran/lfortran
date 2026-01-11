program read_nan_string
  implicit none
  character(len=3) :: nan_string
  character(len=6) :: frmt
  real :: valu

  nan_string = 'NaN'
  frmt = '(f3.3)'

  read(nan_string, frmt) valu

  if (valu == valu) error stop

  print *, 'PASS'
end program read_nan_string
