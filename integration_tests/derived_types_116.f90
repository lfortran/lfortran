module derived_types_116_mod
  implicit none
  type :: t
    real :: v
  contains
    procedure :: get_v
  end type
  type(t), parameter :: x = t(1.0)
contains
  real function get_v(self)
    class(t), intent(in) :: self
    get_v = self%v
  end function
end module

program derived_types_116
  use derived_types_116_mod, only: x
  implicit none
  real :: r
  r = x%get_v()
  if (abs(r - 1.0) > 1e-6) error stop
  print *, r
end program derived_types_116
