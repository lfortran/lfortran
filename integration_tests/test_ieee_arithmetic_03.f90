program test_ieee
  use, intrinsic :: ieee_arithmetic, only: ieee_support_inf, ieee_support_nan
  implicit none
  real(4) :: arr_sp(5) = [1.0, 2.0, 3.0, 4.0, 5.0]
  real(8) :: arr_dp(3) = [1.0d0, 2.0d0, 3.0d0]
  logical :: result_sp(5), result_dp(3)

  result_sp = ieee_support_inf(arr_sp)
  if (.not. all(result_sp)) error stop 
  result_sp = ieee_support_nan(arr_sp)
  if (.not. all(result_sp)) error stop 
  result_dp = ieee_support_inf(arr_dp)
  if (.not. all(result_dp)) error stop 
  result_dp = ieee_support_nan(arr_dp)
  if (.not. all(result_dp)) error stop 
  print *, "test passed"
end program test_ieee
