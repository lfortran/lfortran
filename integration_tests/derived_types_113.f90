! Test: finalize components of derived types at scope exit
! Fortran 2018, clause 7.5.6.2 & 7.5.6.3
!
! When a BLOCK construct terminates, non-pointer non-allocatable
! local variables are finalized. Finalization must recursively
! finalize finalizable components, even if the outer type itself
! has no FINAL subroutine.

module derived_types_113_m
  implicit none

  type :: inner_t
    integer :: val = 0
  contains
    final :: finalize_inner
  end type

  ! outer_t has NO final subroutine, but has a component of a finalizable type
  type :: outer_t
    type(inner_t) :: component
  end type

  logical :: finalized = .false.

contains

  subroutine finalize_inner(self)
    type(inner_t), intent(inout) :: self
    finalized = .true.
    self%val = -1
  end subroutine

end module

program derived_types_113
  use derived_types_113_m
  implicit none

  block
    type(outer_t) :: obj
    obj%component%val = 42
  end block
  ! At end of block, obj goes out of scope.
  ! obj%component (of type inner_t) should be finalized.

  if (.not. finalized) error stop

  print *, "PASS"
end program
