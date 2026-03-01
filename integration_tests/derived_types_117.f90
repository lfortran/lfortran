module derived_types_117_mod
  implicit none
  type :: point
    real :: x, y
  contains
    procedure :: norm
  end type
  type(point), parameter :: origin = point(0.0, 0.0)
  type(point), parameter :: unit_x = point(1.0, 0.0)
  type(point), parameter :: unit_y = point(0.0, 1.0)
contains
  real function norm(self)
    class(point), intent(in) :: self
    norm = sqrt(self%x**2 + self%y**2)
  end function
end module

program derived_types_117
  use derived_types_117_mod, only: origin, unit_x, unit_y, point
  implicit none
  real :: r
  type(point) :: p

  ! Call type-bound procedure on parameter structs
  r = origin%norm()
  if (abs(r) > 1e-6) error stop
  r = unit_x%norm()
  if (abs(r - 1.0) > 1e-6) error stop
  r = unit_y%norm()
  if (abs(r - 1.0) > 1e-6) error stop

  ! Also test with a local variable
  p = point(3.0, 4.0)
  r = p%norm()
  if (abs(r - 5.0) > 1e-6) error stop

  print *, "PASS"
end program derived_types_117
