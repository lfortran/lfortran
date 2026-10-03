module external_15_mod
  implicit none
  integer, parameter :: dp = kind(1d0)
contains
  subroutine root2sqdp(sqrt)
    real(dp), external :: sqrt
    real(dp) :: res
    res = sqrt(2.0_dp)**2
    if (abs(res - 2.0_dp) > 1.0e-14_dp) error stop
    print '(A,F0.16)', 'check sqrt(2.0_dp)**2 = ', res
  end subroutine root2sqdp
end module external_15_mod

program external_15
  use external_15_mod
  implicit none
  intrinsic dsqrt
  call root2sqdp(dsqrt)
end program external_15
