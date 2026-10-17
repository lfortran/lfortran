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

    pure function pure_c() result(c) bind(c)
        character(kind=c_char) :: c
        c = 'q'
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

    subroutine write_value(c)
        character(kind=c_char), value :: c
        character(4) :: buffer
        write(buffer, *) c
        if (trim(adjustl(buffer)) /= 'q') error stop 28
        write(buffer, '(a)') c
        if (buffer /= 'q   ') error stop 29
    end subroutine
end module

program bindc_73
    use bindc_73_mod
    implicit none
    procedure(get_c), pointer :: p
    procedure(source_c), pointer :: source_p
    character(3) :: buffer
    character :: c
    character(:), allocatable :: text

    if (take_c(get_c()) /= iachar('q')) error stop 4
    if (take_value(get_c()) /= iachar('q')) error stop 5
    if (outer_c() /= 'q') error stop 15
    calls = 0
    call take_string(get_c(), 'q')
    if (calls /= 1) error stop 11
    calls = 0
    call take_string(get_c() // 'ab', 'qab')
    if (calls /= 1) error stop 12
    calls = 0
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
    write(buffer, '(a)') get_c()
    if (buffer /= 'q  ') error stop 7
    write(buffer, '(a)') p()
    if (buffer /= 'q  ') error stop 8
    write(buffer, '(a)') source_p()
    if (buffer /= 'rs ') error stop 14
    calls = 0
    if (trim(get_c()) /= 'q') error stop 16
    if (calls /= 1) error stop 17
    calls = 0
    if (adjustl(get_c()) /= 'q') error stop 18
    if (calls /= 1) error stop 19
    calls = 0
    if (adjustr(p()) /= 'q') error stop 20
    if (calls /= 1) error stop 21
    text = trim(p())
    if (text /= 'q' .or. len(text) /= 1) error stop 22
    if (len_trim(get_c()) /= 1) error stop 23
    if (index('aqz', get_c()) /= 2) error stop 24
    calls = 0
    write(buffer, *) get_c()
    if (trim(adjustl(buffer)) /= 'q') error stop 25
    if (calls /= 1) error stop 30
    write(buffer, *) p()
    if (trim(adjustl(buffer)) /= 'q') error stop 26
    if (calls /= 2) error stop 31
    calls = 0
    if (trim(adjustl(get_c())) /= 'q') error stop 32
    if (calls /= 1) error stop 33
    calls = 0
    if (trim(adjustr(p())) /= 'q') error stop 34
    if (calls /= 1) error stop 35
    if (trim(adjustl(pure_c())) /= 'q') error stop 36
    if (trim(adjustr(pure_c())) /= 'q') error stop 37
    calls = 0
    call take_string(trim(adjustl(get_c())), 'q')
    if (calls /= 1) error stop 38
    calls = 0
    text = trim(adjustr(get_c()))
    if (text /= 'q' .or. len(text) /= 1) error stop 39
    if (calls /= 1) error stop 40
    write(buffer, *) source_p()
    if (trim(adjustl(buffer)) /= 'rs') error stop 27
    call write_value('q')
    print *, get_c(), p(), source_p()
end program
