module generic_nopass_01_mod
    implicit none
    private
    public :: command_line_t

    type :: command_line_t
        integer :: base = 1000
    contains
        ! Generic type-bound procedure resolving to `nopass` specifics.
        generic :: flag_value => character_flag_value, integer_flag_value
        procedure, nopass :: character_flag_value, integer_flag_value
        ! Generic mixing a `pass` and a `nopass` specific.
        generic :: combined => pass_add, nopass_len
        procedure :: pass_add
        procedure, nopass :: nopass_len
    end type

contains

    function character_flag_value(flag) result(val)
        character(len=*), intent(in) :: flag
        character(len=:), allocatable :: val
        val = "char:" // flag
    end function

    function integer_flag_value(flag) result(val)
        integer, intent(in) :: flag
        integer :: val
        val = flag + 1
    end function

    integer function pass_add(self, i)
        class(command_line_t), intent(in) :: self
        integer, intent(in) :: i
        pass_add = self%base + i
    end function

    integer function nopass_len(c)
        character(len=*), intent(in) :: c
        nopass_len = len(c)
    end function

end module generic_nopass_01_mod

program generic_nopass_01
    use generic_nopass_01_mod, only: command_line_t
    implicit none
    type(command_line_t) :: cl

    ! Calling a generic that resolves to a `nopass` specific as a function in
    ! an expression previously failed with "Function 'flag_value' not found".
    if (cl%flag_value("--contains") /= "char:--contains") error stop 1

    ! The generic must disambiguate `nopass` specifics by argument type.
    if (cl%flag_value(5) /= 6) error stop 2

    ! A generic mixing `pass` and `nopass` specifics must resolve both.
    if (cl%combined(5) /= 1005) error stop 3
    if (cl%combined("hello") /= 5) error stop 4

    print *, "ok"
end program generic_nopass_01
