program read_nan_string
  use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
  implicit none
  character(len=3) :: nan_string
  real :: valu

  nan_string = 'NaN'
  read(nan_string, '(f3.3)') valu
  if (.not. ieee_is_nan(valu)) error stop "wrong value read"

  print *, 'test passed!'
end program read_nan_string