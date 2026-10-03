! Test re-export of use-associated symbols with private default.
! Symbols imported via `use` and explicitly made `public` must be
! visible when the re-exporting module is itself `use`d.

module modules_71_kinds
  use iso_fortran_env, only: int8, int32
  implicit none
  private
  public :: int8, int32
end module modules_71_kinds

module modules_71_util
  use modules_71_kinds
  implicit none
contains
  subroutine set_first(a, val)
    integer(int8), intent(inout) :: a(:)
    integer(int32), intent(in) :: val
    a(1) = int(val, int8)
  end subroutine
end module modules_71_util

program modules_71
  use modules_71_util
  implicit none
  integer(1) :: x(3)
  x = [1, 2, 3]
  call set_first(x, 42)
  if (x(1) /= 42) error stop
  if (x(2) /= 2) error stop
  if (x(3) /= 3) error stop
end program modules_71
