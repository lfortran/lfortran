program test_ieee_infnan
  use,intrinsic:: ieee_arithmetic
  implicit none
  integer, parameter :: dp = kind(1d0)
  real(dp) :: xdp(4)

  xdp = [ ieee_value(1d0, ieee_positive_inf), &
           ieee_value(1d0, ieee_negative_inf), &
           ieee_value(1d0, ieee_quiet_nan), &
           ieee_value(1d0, ieee_signaling_nan) ]

  if (.not. isinfdp(xdp(1))) error stop
  if (.not. isinfdp(xdp(2))) error stop
  if (    isinfdp(xdp(3))) error stop
  if (    isinfdp(xdp(4))) error stop

  if (.not. isposinfdp(xdp(1))) error stop
  if (    isposinfdp(xdp(2))) error stop
  if (    isposinfdp(xdp(3))) error stop
  if (    isposinfdp(xdp(4))) error stop

  if (    isneginfdp(xdp(1))) error stop
  if (.not. isneginfdp(xdp(2))) error stop
  if (    isneginfdp(xdp(3))) error stop
  if (    isneginfdp(xdp(4))) error stop

  if (    isnandp(xdp(1))) error stop
  if (    isnandp(xdp(2))) error stop
  if (.not. isnandp(xdp(3))) error stop
  if (.not. isnandp(xdp(4))) error stop

  if (    isfinitedp(xdp(1))) error stop
  if (    isfinitedp(xdp(2))) error stop
  if (    isfinitedp(xdp(3))) error stop
  if (    isfinitedp(xdp(4))) error stop

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

end program test_ieee_infnan
