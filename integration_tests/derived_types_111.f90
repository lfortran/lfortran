! Test: finalize intent(out) dummy argument on procedure entry
! Fortran 2018, clause 7.5.6.3, paragraph 7

module derived_types_111_m
  implicit none
  integer :: finalizations = 0

  type :: obj_t
    integer :: dummy = 0
  contains
    final :: count_finalizations
  end type

contains

  subroutine count_finalizations(self)
    type(obj_t), intent(inout) :: self
    finalizations = finalizations + 1
    self%dummy = 0
  end subroutine

end module

program derived_types_111
  use derived_types_111_m
  implicit none

  type(obj_t) :: obj
  integer :: tally_before

  obj%dummy = 1
  tally_before = finalizations

  call set_intent_out(obj)

  if (finalizations - tally_before /= 1) then
    error stop 1
  end if

contains

  subroutine set_intent_out(x)
    type(obj_t), intent(out) :: x
    x%dummy = 42
  end subroutine

end program
