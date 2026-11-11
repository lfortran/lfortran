module lfortran_intrinsic_ieee_exceptions
    implicit none

    type ieee_flag_type
        integer :: value
    end type

    type(ieee_flag_type), parameter :: ieee_invalid = ieee_flag_type(1)
    type(ieee_flag_type), parameter :: ieee_overflow = ieee_flag_type(2)
    type(ieee_flag_type), parameter :: ieee_divide_by_zero = ieee_flag_type(3)
    type(ieee_flag_type), parameter :: ieee_underflow = ieee_flag_type(4)
    type(ieee_flag_type), parameter :: ieee_inexact = ieee_flag_type(5)

contains

    ! Exception flags are not yet wired to the hardware status word.
    elemental subroutine ieee_get_flag(flag, flag_value)
        type(ieee_flag_type), intent(in) :: flag
        logical, intent(out) :: flag_value
        flag_value = .false.
    end subroutine

    elemental subroutine ieee_set_flag(flag, flag_value)
        type(ieee_flag_type), intent(in) :: flag
        logical, intent(in) :: flag_value
    end subroutine

    elemental subroutine ieee_get_halting_mode(flag, halting)
        type(ieee_flag_type), intent(in) :: flag
        logical, intent(out) :: halting
        halting = .false.
    end subroutine

    elemental subroutine ieee_set_halting_mode(flag, halting)
        type(ieee_flag_type), intent(in) :: flag
        logical, intent(in) :: halting
    end subroutine

end module
