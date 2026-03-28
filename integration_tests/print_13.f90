program print_13
    implicit none
    integer, parameter :: sp = selected_real_kind(6)
    complex(sp) :: c
    character(len=:), allocatable :: s

    c = (109.0_sp, 20.0_sp)
    s = to_string_c_sp(c)
    if (len(s) /= 0) error stop
    print *, to_string_c_sp(c)

    contains

    function to_string_c_sp(value) result(string)
        complex(sp), intent(in) :: value
        character(len=:), allocatable :: string
    end function to_string_c_sp

end program print_13
