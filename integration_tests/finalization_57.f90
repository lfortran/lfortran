! Compiled with -g: a BLOCK construct whose statements set no debug location
! (none, or only IF, EXIT and RETURN) finalizes its local at its end and on
! every branch out of it, and each of those finalization calls has one.
module finalization_57_mod
implicit none
integer :: finalized = 0
type :: t
contains
    final :: finalize
end type
contains
subroutine finalize(x)
    type(t), intent(inout) :: x
    finalized = finalized + 1
end subroutine
subroutine empty()
    block
        type(t) :: y
    end block
end subroutine
subroutine leave(how)
    integer, intent(in) :: how
    outer: block
        type(t) :: y
        block
            type(t) :: z
            if (how == 1) return
            if (how == 2) exit outer
        end block
    end block outer
end subroutine
end module

program finalization_57
use finalization_57_mod
implicit none
integer :: how
call empty()
print *, finalized
if (finalized /= 1) error stop
do how = 1, 3
    finalized = 0
    call leave(how)
    print *, how, finalized
    if (finalized /= 2) error stop
end do
end program
