program struct_param_char_array_finalize_12664
    implicit none

    character(len=1), parameter :: letters(4) = ['a', 'b', 'c', 'd']

    type calendar
        character(len=1) :: chars(4) = letters
    end type calendar

    type(calendar), parameter :: calen = calendar()

    print *, calen%chars
end program struct_param_char_array_finalize_12664
