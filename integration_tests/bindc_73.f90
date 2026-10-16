module bindc_73_mod
    use iso_c_binding, only: c_char, c_int
    implicit none
    integer :: calls = 0
contains
    function get_c() result(c) bind(c)
        character(kind=c_char) :: c
        calls = calls + 1
        c = 'q'
    end function

    integer function take_c(c) result(i)
        character, intent(in) :: c
        i = iachar(c)
    end function

    subroutine take_string(c, expected)
        character(*), intent(in) :: c, expected
        if (c /= expected) error stop 1
        if (len(c) /= len(expected)) error stop 2
    end subroutine

    function outer_c() result(c) bind(c)
        character(kind=c_char) :: c
        c = inner_c()
    contains
        function inner_c() result(c)
            character(3) :: c
            c = 'qrs'
        end function
    end function
end module

program bindc_73
    use bindc_73_mod
    implicit none
    if (take_c(get_c()) /= iachar('q')) error stop 4
    if (outer_c() /= 'q') error stop 15
    calls = 0
    call take_string(get_c(), 'q')
    if (calls /= 1) error stop 11
    calls = 0
    call take_string(get_c() // 'ab', 'qab')
    if (calls /= 1) error stop 12
    call take_string(repeat(get_c(), 3), 'qqq')
end program
