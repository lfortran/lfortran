! Test: finalize non-pointer non-allocatable local variable at END
! Fortran 2018, clause 7.5.6.3, paragraph 3

module derived_types_110_m
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

  subroutine my_sub()
    type(obj_t) :: local_obj
    local_obj%dummy = 1
  end subroutine  ! local_obj should be finalized here

end module

program derived_types_110
  use derived_types_110_m
  implicit none
  integer :: tally_before

  tally_before = finalizations
  call my_sub()

  if (finalizations - tally_before == 1) then
    print *, "Test passed"
  else
    print *, "Test FAILED"
    error stop 1
  end if
end program
