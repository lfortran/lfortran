! Specific intrinsics of two different kinds (sqrt for real(4), dsqrt for
! real(8)) passed to module procedures whose dummy procedures are declared
! `real(sp), external` and `real(dp), external` (#13166).
module external_40_mod
  implicit none
  integer, parameter :: sp = kind(1.0), dp = kind(1d0)
contains
  subroutine f2sqsp(f, res)
    real(sp), external :: f
    real(sp), intent(out) :: res
    res = f(1/0.7_sp)**2
    print "(F10.7)", res
  end subroutine f2sqsp

  subroutine f2sqdp(f, res)
    real(dp), external :: f
    real(dp), intent(out) :: res
    res = f(1/0.7_dp)**2
    print "(F19.16)", res
  end subroutine f2sqdp
end module external_40_mod

program external_40
  use external_40_mod
  implicit none
  intrinsic sqrt, dsqrt
  real(sp) :: rs
  real(dp) :: rd
  call f2sqsp(sqrt, rs)
  call f2sqdp(dsqrt, rd)
  if (abs(rs - 1/0.7_sp) > 1e-5_sp) error stop
  if (abs(rd - 1/0.7_dp) > 1e-13_dp) error stop
end program external_40
