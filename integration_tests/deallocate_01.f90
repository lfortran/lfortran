module m_deallocate_01
implicit none

type :: object_t
    integer :: dummy = 0
contains
    final :: count_finalizations
end type

integer :: finalizations = 0

contains

subroutine count_finalizations(self)
    type(object_t), intent(inout) :: self
    finalizations = finalizations + 1
    self%dummy = 0
end subroutine

end module

program deallocate_01
! Test: finalizer is called on explicit DEALLOCATE (Fortran 2018 §7.5.6.3)
use m_deallocate_01
implicit none
type(object_t), allocatable :: obj
integer :: count0

count0 = finalizations
allocate(obj)
obj%dummy = 42
deallocate(obj)

if (finalizations - count0 /= 1) error stop

end program
