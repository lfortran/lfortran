module modules_67_origin
  implicit none

  type :: pair_t
    integer :: x = 0
  end type

  interface operator(.check.)
    module procedure pair_check
  end interface

contains

  pure logical function pair_check(a, b)
    type(pair_t), intent(in) :: a, b
    pair_check = (a%x == b%x)
  end function

end module
