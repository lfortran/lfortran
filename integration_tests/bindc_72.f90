program bindc_72
    use iso_c_binding, only: c_char, c_int
    implicit none

    if (length_three() /= 3) error stop 1
    if (length_one() /= 1) error stop 2
    if (character_result() /= 'b') error stop 3
contains
    integer(c_int) function length_three() result(answer) bind(c)
        type :: local_type
            character(len=3) :: answer
        end type
        type(local_type) :: value
        value%answer = 'abc'
        answer = len(value%answer)
    end function

    integer(c_int) function length_one() result(answer) bind(c)
        type :: local_type
            character :: answer
        end type
        type(local_type) :: value
        value%answer = 'a'
        if (value%answer /= 'a') error stop 4
        answer = len(value%answer)
    end function

    function character_result() result(c) bind(c)
        type :: local_type
            character(len=3) :: c
        end type
        type(local_type) :: value
        character(kind=c_char) :: c
        value%c = 'abc'
        c = value%c(2:2)
    end function
end program
