! Test: debug info with derived type FINAL subroutine
! Verifies that -g flag works when a derived type with a
! FINAL subroutine is used as a local variable.

module derived_types_123_mod
  implicit none
  type, public :: container
    integer :: val = 0
  contains
    final :: dealloc_container
  end type container
contains
  subroutine dealloc_container(this)
    type(container), intent(inout) :: this
    this%val = 0
  end subroutine dealloc_container
end module derived_types_123_mod

program derived_types_123
  use derived_types_123_mod
  implicit none
  type(container) :: c
  c%val = 42
  if (c%val /= 42) error stop
  print *, "PASS"
end program derived_types_123
