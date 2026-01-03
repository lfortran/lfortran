program intrinsics_84
  use iso_fortran_env, only: sp => real32, dp => real64
  implicit none
  integer, parameter :: i1 = minexponent(0.131_sp)
  integer, parameter :: i2 = minexponent(0.12819_dp)
  integer, parameter :: ar1 = minexponent([0.131_sp, 0.12819_sp, 0.0_sp])
  integer, parameter :: ar2 = minexponent([1.24_dp, 1.119_dp, 0.0_dp])
  real(kind=4) :: x
  real(kind=8) :: y
  real(sp) :: arr1(3) = [0.131_sp, 0.12819_sp, 0.0_sp]
  real(dp) :: arr2(3) = [1.24_dp, 1.239_dp, 0.0_dp]
  integer :: result

  print *, i1
  if (i1 /= -125) error stop
  print *, i2
  if (i2 /= -1021) error stop
  print *, ar1
  if (ar1 /= -125) error stop
  print *, ar2
  if (ar2 /= -1021) error stop

  result = minexponent(x)
  print *, result
  if (result /= -125) error stop

  result = minexponent(y)
  print *, result
  if (result /= -1021) error stop

  result = minexponent(4.235)
  print *, result
  if (result /= -125) error stop

  result = minexponent(4.235d0)
  print *, result
  if (result /= -1021) error stop

  print *, minexponent(arr1)
  if (minexponent(arr1) /= -125) error stop
  print *, minexponent(arr2)
  if (minexponent(arr2) /= -1021) error stop

end program intrinsics_84
