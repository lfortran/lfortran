program subroutines_20
    type :: regex_token
        character(len=:), allocatable :: ccl
    end type regex_token

    type :: regex_pattern
        type(regex_token) :: pattern
    end type regex_pattern

    type(regex_pattern) :: my_pattern
    call temp(my_pattern%pattern)

    if (my_pattern%pattern%ccl /= "abc") error stop
contains
    subroutine temp(pattern)
        type(regex_token), intent(out) :: pattern
        pattern%ccl = 'abc'
    end subroutine temp
end program
