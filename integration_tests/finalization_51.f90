! A final procedure of a local of a non-recursive host invokes a procedure
! pointer that the host pointed at one of its internal procedures, which
! reads the local itself and a variable that a BLOCK construct of the host
! sets: the local is finalized before the host gives its frame slot back.
module finalization_51_mod
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
contains
subroutine finalize(x)
    type(t), intent(inout) :: x
    if (associated(saved)) seen = saved() + x%v
end subroutine
subroutine host(n)
    integer, intent(in) :: n
    type(t) :: x
    integer :: k
    k = 0
    block
        integer :: j
        j = 2
        x%v = n
        k = j
    end block
    saved => get
contains
    integer function get()
        get = 10 * x%v + k
    end function
end subroutine
end module

program finalization_51
use finalization_51_mod
implicit none
call host(7)
print *, seen
if (seen /= 79) error stop
end program
