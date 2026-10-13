module traits_runtime_character_separate_01_contracts_m
    implicit none

    ! Two members of different constant lengths: each has its own closed
    ! runtime slot.
    abstract interface :: ICode
        character(len=3) | character(len=5)
    end interface ICode

    ! The smallest element of x, but not below floor.
    abstract interface :: IPick
        function pick{ICode :: T}(x, floor) result(r)
            type(T), intent(in) :: x(:)
            type(T), intent(in) :: floor
            type(T)             :: r
        end function pick
    end interface IPick

    ! Ordinary messages with character results of two lengths.
    abstract interface :: ILabel
        function label(code, width) result(s)
            character(len=*), intent(in) :: code
            integer,          intent(in) :: width
            character(len=5)             :: s
        end function label
        function first_letter(text) result(c)
            character(len=*), intent(in) :: text
            character                    :: c
        end function first_letter
    end interface ILabel

    ! Implemented by the separately compiled provider; clients need only this
    ! contract module.
    interface
        subroutine make_picker(choice, object)
            import :: IPick
            integer, intent(in) :: choice
            class(IPick), allocatable, intent(out) :: object
        end subroutine make_picker

        subroutine make_label(object)
            import :: ILabel
            class(ILabel), allocatable, intent(out) :: object
        end subroutine make_label
    end interface
end module traits_runtime_character_separate_01_contracts_m
