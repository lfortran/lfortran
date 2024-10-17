module mod_elemental_function_scalar_array_arg
    implicit none
    contains

    !> Returns an integer
    elemental function count_char_char(string, pattern) result(res)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: pattern
        integer :: res

        res = 1
    end function count_char_char

end module

program elemental_function_scalar_array_arg
    use mod_elemental_function_scalar_array_arg
    implicit none
    character(len=128) :: string
    integer :: count(3)

    string = "How much wood would a woodchuck chuck if a woodchuck could chuck wood?"
    print *, count_char_char(string, ["would", "chuck", "could"])
    count = count_char_char(string, ["would", "chuck", "could"])
    if (any(count /= [1, 1, 1])) error stop
end program
