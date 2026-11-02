! The final procedure of an allocatable local of a host invokes a procedure
! pointer that the host pointed at one of its internal procedures: the
! local is deallocated and finalized before the host gives its frame slot
! back.
module finalization_54_mod
implicit none
abstract interface
    integer function getter()
    end function
end interface
procedure(getter), pointer :: saved => null()
integer :: seen = 0
type :: t
    integer :: v = 0
contains
    final :: finalize
end type
type :: holder
    type(t) :: item
end type
contains
subroutine finalize(x)
    type(t), intent(inout) :: x
    if (associated(saved)) seen = seen + saved() + x%v
end subroutine
recursive subroutine host(n)
    integer, intent(in) :: n
    type(t), allocatable :: x
    type(holder), allocatable :: h(:)
    allocate(x)
    x%v = 100
    allocate(h(2))
    h(1)%item%v = 1000
    saved => get
contains
    integer function get()
        get = n
    end function
end subroutine
end module

program finalization_54
use finalization_54_mod
implicit none
call host(9)
print *, seen
if (seen /= 9 + 100 + 9 + 1000 + 9) error stop
end program
