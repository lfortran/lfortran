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

contains

    pure subroutine assign_string_char(lhs, rhs)
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

module stdlib_string_use
use stdlib_string_type, only: string_type
implicit none

contains

pure function chomp_string(string) result(chomped_string)
    ! Avoid polluting the module scope and use the assignment only in this scope
    use stdlib_string_type, only : assignment(=)
    type(string_type), intent(in) :: string
    type(string_type) :: chomped_string
    integer :: last
end function chomp_string

end module

program stdlib_string
end program