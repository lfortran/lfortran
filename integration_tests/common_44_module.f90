! The bug needs common_44_legacy.f90 compiled after this file: its COMMON
! declaration then replaced the layout common_44_mod was compiled against.
! common_44_legacy.f90 uses common_44_order_mod only to force that order.
module common_44_order_mod
    implicit none
    integer, parameter :: expected = 17
end module common_44_order_mod

module common_44_mod
    implicit none
    integer, target :: a
    common /common_44_blk/ a
contains
    subroutine bind(x)
        integer, pointer :: x
        x => a
    end subroutine bind
end module common_44_mod
