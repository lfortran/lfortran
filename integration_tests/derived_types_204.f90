module module_derived_type_char_int_char
    implicit none

    type :: t
        character :: a = 'x'
        integer :: b = 42
        character :: c = 'z'
    end type

    type(t) :: ma

contains

    subroutine check_module_variable()
        if (ma%a /= 'x') error stop
        if (ma%b /= 42) error stop
        if (ma%c /= 'z') error stop
    end subroutine

    subroutine check_save_variable()
        type(t), save :: saved = t('l', 7, 'r')
        if (saved%a /= 'l') error stop
        if (saved%b /= 7) error stop
        if (saved%c /= 'r') error stop
    end subroutine

end module

program main
    use module_derived_type_char_int_char, only: check_module_variable, check_save_variable
    implicit none

    call check_module_variable()
    call check_save_variable()
end program
