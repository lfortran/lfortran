! A final procedure of a local of a recursive host invokes a procedure
! pointer that the host pointed at one of its internal procedures: the
! local is finalized while the instance still holds the frame slot of the
! thunk, so the internal procedure reads the variables of that instance.
module finalization_50_mod
implicit none
abstract interface
    integer function getter()
    end function
end interface
procedure(getter), pointer :: saved => null()
integer :: seen = 0
integer :: finalized = 0
type :: t
contains
    final :: finalize
end type
contains
subroutine finalize(x)
    type(t), intent(inout) :: x
    finalized = finalized + 1
    if (associated(saved)) seen = saved()
end subroutine
recursive subroutine host(n)
    integer, intent(in) :: n
    type(t) :: x
    saved => get
contains
    integer function get()
        get = n
    end function
end subroutine
end module

program finalization_50
use finalization_50_mod
implicit none
call host(9)
print *, seen
if (seen /= 9) error stop
if (finalized /= 1) error stop
call host(4)
print *, seen
if (seen /= 4) error stop
if (finalized /= 2) error stop
end program
