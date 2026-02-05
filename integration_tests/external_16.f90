! Test external real function passed as argument
! Related to issue #6783
module external_16_mod
  implicit none
  integer, parameter:: dp=kind(1d0)
contains
  subroutine root2sqdp(sqrt)
    real(dp),external::sqrt
    real(dp) :: result
    result = sqrt(2.0_dp)**2
    if (abs(result - 2.0_dp) > 1.0d-14) error stop
  end subroutine root2sqdp
end module external_16_mod

program external_16
  use external_16_mod
  implicit none
  intrinsic dsqrt
  call root2sqdp(dsqrt)
end program external_16
