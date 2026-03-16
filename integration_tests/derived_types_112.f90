! Test: finalize LHS of intrinsic assignment
! Fortran 2018, clause 7.5.6.3, paragraph 1

module derived_types_112_m
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

program derived_types_112
  use derived_types_112_m
  implicit none
  type(obj_t) :: lhs, rhs

  lhs%dummy = 0
  rhs%dummy = 1
  finalizations = 0
  lhs = rhs  ! This should finalize lhs before the assignment

  if (finalizations /= 1) error stop

  ! Verify the assignment itself still works
  if (lhs%dummy /= 1) error stop

  print *, "PASS"
end program
