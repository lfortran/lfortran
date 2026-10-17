! The variables of common_54_mod are in /common_54_blk/. External procedures
! that do not use the module declare the block with other names.
module common_54_mod
    implicit none
    integer, target :: a
    integer :: c(2)
    common /common_54_blk/ a, c
contains
    subroutine bind(x)
        integer, pointer :: x
        x => a
    end subroutine bind
end module common_54_mod
