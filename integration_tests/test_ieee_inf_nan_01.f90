program test_ieee_inf_nan_01
  use, intrinsic :: ieee_arithmetic
  implicit none
  integer, parameter :: dp = kind(1d0)
  real(dp) :: xdp(4)

  xdp = [ ieee_value(1d0, ieee_positive_inf), &
       ieee_value(1d0, ieee_negative_inf), &
       ieee_value(1d0, ieee_quiet_nan), &
       ieee_value(1d0, ieee_signaling_nan) ]

  if (.not. all(isinfdp(xdp)    .eqv. [.true.,  .true.,  .false., .false.])) error stop
  if (.not. all(isposinfdp(xdp) .eqv. [.true.,  .false., .false., .false.])) error stop
  if (.not. all(isneginfdp(xdp) .eqv. [.false., .true.,  .false., .false.])) error stop
  if (.not. all(isnandp(xdp)    .eqv. [.false., .false., .true.,  .true. ])) error stop
  if (.not. all(isfinitedp(xdp) .eqv. [.false., .false., .false., .false.])) error stop

contains

  elemental logical function isnandp(x)
    real(dp), intent(in) :: x
    isnandp = (x /= x)
  end function isnandp

  elemental logical function isinfdp(x)
    real(dp), intent(in) :: x
    isinfdp = (abs(x) > huge(x))
  end function isinfdp

  elemental logical function isposinfdp(x)
    real(dp), intent(in) :: x
    isposinfdp = (x > huge(x))
  end function isposinfdp

  elemental logical function isneginfdp(x)
    real(dp), intent(in) :: x
    isneginfdp = (x < -huge(x))
  end function isneginfdp

  elemental logical function isfinitedp(x)
    real(dp), intent(in) :: x
    isfinitedp = (abs(x) <= huge(x))
  end function isfinitedp

end program test_ieee_inf_nan_01
