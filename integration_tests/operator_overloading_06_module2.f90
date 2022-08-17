module stdlib_strings
use stdlib_string_type, only: string_type, char
implicit none

    interface chomp
        module procedure :: chomp_set_char_char
        module procedure :: chomp_set_string_char
    end interface chomp

contains

pure function chomp_set_string_char(string, set) result(chomped_string)
    ! Avoid polluting the module scope and use the assignment only in this scope
    use stdlib_string_type, only : assignment(=)
    type(string_type), intent(in) :: string
    character(len=1), intent(in) :: set(:)
    type(string_type) :: chomped_string

    chomped_string = chomp(char(string), set)
end function chomp_set_string_char

pure function chomp_set_char_char(string, set) result(chomped_string)
    character(len=*), intent(in) :: string
    character(len=1), intent(in) :: set(:)
    character(len=:), allocatable :: chomped_string
    integer :: last

end function chomp_set_char_char

end module