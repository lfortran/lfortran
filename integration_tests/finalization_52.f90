! A host returns early, from its body, from a loop and from a BLOCK
! construct, after pointing a procedure pointer at one of its internal
! procedures: its finalizable local is finalized before the host gives its
! frame slot back, on every return.
module finalization_52_mod
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
subroutine host(n, how)
    integer, intent(in) :: n, how
    type(t) :: x
    integer :: i
    saved => get
    if (how == 1) return
    do i = 1, 3
        if (how == 2 .and. i == 2) return
    end do
    block
        type(t) :: y
        if (how == 3) return
    end block
    if (how == 4) then
        return
    end if
contains
    integer function get()
        get = n
    end function
end subroutine
end module

program finalization_52
use finalization_52_mod
implicit none
integer :: how
do how = 1, 5
    seen = 0
    finalized = 0
    call host(10 * how, how)
    print *, how, seen, finalized
    if (seen /= 10 * how) error stop
    if (how <= 2) then
        if (finalized /= 1) error stop
    else
        if (finalized /= 2) error stop
    end if
end do
end program
