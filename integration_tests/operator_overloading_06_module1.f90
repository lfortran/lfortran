module stdlib_string_type
    type :: string_type
        ! Use the sequence statement below as a hack to prevent extending this type.
        ! It is not used for storage association.
        sequence
        character(len=:), allocatable :: raw
    end type string_type

    interface assignment(=)
        module procedure :: assign_string_char
    end interface assignment(=)

    interface len
        module procedure :: len_string
    end interface len

    interface char
        module procedure :: char_string
        module procedure :: char_string_pos
        module procedure :: char_string_range
    end interface char

contains

    pure subroutine assign_string_char(lhs, rhs)
        type(string_type), intent(inout) :: lhs
        character(len=*), intent(in) :: rhs
        lhs%raw = rhs
    end subroutine assign_string_char

    pure function char_string(string) result(character_string)
        type(string_type), intent(in) :: string
        character(len=len(string)) :: character_string
    end function char_string

    elemental function char_string_pos(string, pos) result(character_string)
        type(string_type), intent(in) :: string
        integer, intent(in) :: pos
        character(len=1) :: character_string
    end function char_string_pos

    pure function char_string_range(string, start, last) result(character_string)
        type(string_type), intent(in) :: string
        integer, intent(in) :: start
        integer, intent(in) :: last
        character(len=last-start+1) :: character_string
    end function char_string_range

    elemental function len_string(string) result(length)
        type(string_type), intent(in) :: string
        integer :: length
    end function len_string
end module