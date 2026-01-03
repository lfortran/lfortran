program intrinsics_85
  use iso_fortran_env, only: sp => real32, dp => real64
  implicit none
  integer, parameter :: i1 = maxexponent(0.131_sp)
  integer, parameter :: i2 = maxexponent(0.12819_dp)
  integer, parameter :: ar1 = maxexponent([0.131_sp, 0.12819_sp, 0.0_sp])
  integer, parameter :: ar2 = maxexponent([1.24_dp, 1.119_dp, 0.0_dp])
  real(kind=4) :: x
  real(kind=8) :: y
  real(sp) :: arr1(3) = [0.131_sp, 0.12819_sp, 0.0_sp]
  real(dp) :: arr2(3) = [1.24_dp, 1.239_dp, 0.0_dp]
  integer :: result

  print *, i1
  if (i1 /= 128) error stop
  print *, i2
  if (i2 /= 1024) error stop
  print *, ar1
  if (ar1 /= 128) error stop
  print *, ar2
  if (ar2 /= 1024) error stop

  result = maxexponent(x)
  print *, 'maxexponent(x) = ', result
  if (result /= 128) error stop

  result = maxexponent(y)
  print *, 'maxexponent(y) = ', result
  if (result /= 1024) error stop

  result = maxexponent(-214.91)
  print *, 'maxexponent(-214.91) = ', result
  if (result /= 128) error stop

  result = maxexponent(214.91D0)
  print *, 'maxexponent(214.91D0) = ', result
  if (result /= 1024) error stop

  print *, maxexponent(arr1)
  if (maxexponent(arr1) /= 128) error stop
  print *, maxexponent(arr2)
  if (maxexponent(arr2) /= 1024) error stop

end program intrinsics_85
