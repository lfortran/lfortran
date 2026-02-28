! Test: generic procedure with keyword arg resolves to correct overload
! when an earlier overload has a required argument that is not supplied.
! Regression test for: ICE AssertFailed: f != nullptr in argument_types_match
! when a required arg is nullptr (not provided) for a non-matching overload.

module generic_name_04_mod
  implicit none

  interface foo
    ! First overload: center is REQUIRED, mask is optional.
    ! A call that omits center should skip this overload.
    module procedure foo_center
    ! Second overload: center is optional, mask(:) is REQUIRED.
    ! A call foo(y, mask=m) should resolve to this.
    module procedure foo_mask
  end interface

contains

  real function foo_center(x, center, mask) result(res)
    real, intent(in) :: x(:)
    real, intent(in) :: center
    logical, intent(in), optional :: mask
    res = sum(x) + center
  end function

  real function foo_mask(x, center, mask) result(res)
    real, intent(in) :: x(:)
    real, intent(in), optional :: center
    logical, intent(in) :: mask(:)
    res = sum(x, mask=mask)
  end function

end module

program generic_name_04
  use generic_name_04_mod
  implicit none
  real    :: y(6) = [1., 2., 3., 4., 5., 6.]
  logical :: m(6)
  real    :: r
  m = y > 3.
  ! Should resolve to foo_mask: sum of elements > 3 = 4+5+6 = 15
  r = foo(y, mask=m)
  print *, r
  if (abs(r - 15.0) > 1e-5) error stop
end program
