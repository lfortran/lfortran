! Declares the COMMON block of common_44_mod with another member name. The use
! of common_44_order_mod makes this file compile after common_44_module.f90,
! so this declaration comes after common_44_mod was compiled; it must not
! change the layout the modfiles of common_44_mod refer to.
module common_44_legacy_mod
    use common_44_order_mod, only: expected
    implicit none
contains
    subroutine set_value()
        external common_44_legacy
        call common_44_legacy(expected)
    end subroutine set_value
end module common_44_legacy_mod

subroutine common_44_legacy(value)
    implicit none
    integer, intent(in) :: value
    integer :: b
    common /common_44_blk/ b
    b = value
end subroutine common_44_legacy
