module interface_01_mod
  implicit none

  interface func
    module function e(x)
      implicit none
      real, intent(in) :: x
      real :: e
    end function e
  end interface

contains
  module function e(x)
    integer, intent(in) :: x
    real :: e
    e = x * 2.0
  end function

end module interface_01_mod
