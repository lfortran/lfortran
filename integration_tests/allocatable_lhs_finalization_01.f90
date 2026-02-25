! Finalize an allocated allocatable LHS before intrinsic assignment
! (Fortran 2018, 7.5.6.3, paragraph 1).
module allocatable_lhs_finalization_01_m
  implicit none
  integer :: finalizations = 0
  type :: t
    integer :: dummy = 0
  contains
    final :: finalize_t
  end type
contains
  subroutine finalize_t(self)
    type(t), intent(inout) :: self
    self%dummy = 0
    finalizations = finalizations + 1
  end subroutine
end module

program allocatable_lhs_finalization_01
  use allocatable_lhs_finalization_01_m
  implicit none
  type(t), allocatable :: lhs
  type(t) :: rhs
  integer :: count_before

  allocate(lhs)
  count_before = finalizations
  lhs = rhs  ! Should finalize old lhs before assignment (7.5.6.3 para 1)
  if (finalizations - count_before /= 1) error stop
end program
