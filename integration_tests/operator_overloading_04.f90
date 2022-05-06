module stdlib_string_type
    type :: string_type
        ! Use the sequence statement below as a hack to prevent extending this type.
        ! It is not used for storage association.
        sequence
        private
        character(len=:), allocatable :: raw
    end type string_type

    interface assignment(=)
        module procedure :: assign_string_char
    end interface assignment(=)

contains

    subroutine assign_string_char(lhs, rhs)
        type(string_type), intent(inout) :: lhs
        character(len=*), intent(in) :: rhs
        lhs%raw = rhs
    end subroutine assign_string_char

    elemental function trim_string(string) result(trimmed_string)
        type(string_type), intent(in) :: string
        type(string_type) :: trimmed_string

        trimmed_string = string%raw

    end function trim_string
end module

program stdlib_string
end program