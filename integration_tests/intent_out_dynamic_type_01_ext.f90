! An extension of Base in a unit compiled after it, the only one that
! creates objects of the extension (intent_out_dynamic_type_01).
module intent_out_dynamic_type_01_ext
    use intent_out_dynamic_type_01_base, only: Base
    implicit none
    integer :: ext_finals = 0, seen_extra = -1
    logical :: seen_data = .false.
    type, extends(Base) :: Ext
        integer :: extra = 11
        integer, allocatable :: data(:)
    contains
        final :: ext_final
    end type
contains
    subroutine ext_final(self)
        type(Ext), intent(inout) :: self
        ext_finals = ext_finals + 1
        seen_extra = self%extra
        seen_data = allocated(self%data)
    end subroutine

    subroutine make(p, extra)
        class(Base), allocatable, intent(out) :: p
        integer, intent(in) :: extra
        allocate(Ext :: p)
        select type (p)
        type is (Ext)
            p%n = 1
            p%extra = extra
            allocate(p%data(3))
        end select
    end subroutine

    logical function is_reset(p)
        class(Base), intent(in) :: p
        is_reset = .false.
        select type (p)
        type is (Ext)
            is_reset = p%n == 7 .and. p%extra == 11 .and. .not. allocated(p%data)
        end select
    end function
end module
