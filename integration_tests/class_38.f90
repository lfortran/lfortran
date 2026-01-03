program class_38
     type :: regex_token
        character(len=:), allocatable :: ccl
    end type regex_token

    type :: regex_pattern
        type(regex_token), dimension(2) :: pattern
    end type regex_pattern

    type(regex_pattern) :: my_pattern
    call temp(my_pattern%pattern)

    if (my_pattern%pattern(1)%ccl /= 'abc' .or. my_pattern%pattern(2)%ccl /= 'def') error stop
contains 
    subroutine temp(pattern)
        type(regex_token), dimension(:), intent(out) :: pattern
        pattern(1)%ccl = 'abc'
        pattern(2)%ccl = 'def'
    end subroutine temp
end program
