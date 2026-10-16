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

    integer(c_int) function take_value(c) result(i) bind(c)
        character(kind=c_char), value :: c
        i = iachar(c)
    end function

    subroutine take_string(c, expected)
        character(*), intent(in) :: c, expected
        if (c /= expected) error stop 1
        if (len(c) /= len(expected)) error stop 2
    end subroutine

    subroutine callback(p)
        procedure(get_c) :: p
        character :: c
        c = p()
        if (c /= 'q') error stop 3
    end subroutine

    subroutine pointer_callback(p)
        procedure(get_c), pointer :: p
        character :: c
        c = p()
        if (c /= 'q') error stop 9
    end subroutine

    function source_c() result(c)
        character(2) :: c
        c = 'rs'
    end function

    subroutine source_callback(p)
        procedure(source_c) :: p
        character(2) :: c
        c = p()
        if (c /= 'rs') error stop 10
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
    procedure(get_c), pointer :: p
    procedure(source_c), pointer :: source_p
    character :: c
    if (take_c(get_c()) /= iachar('q')) error stop 4
    if (take_value(get_c()) /= iachar('q')) error stop 5
    if (outer_c() /= 'q') error stop 15
    calls = 0
    call take_string(get_c(), 'q')
    if (calls /= 1) error stop 11
    calls = 0
    call take_string(get_c() // 'ab', 'qab')
    if (calls /= 1) error stop 12
    call take_string(repeat(get_c(), 3), 'qqq')
    p => get_c
    c = p()
    if (c /= 'q') error stop 6
    call callback(get_c)
    call callback(p)
    call pointer_callback(p)
    source_p => source_c
    call take_string(source_p(), 'rs')
    call source_callback(source_c)
    call source_callback(source_p)
end program
