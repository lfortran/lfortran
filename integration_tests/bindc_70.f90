module bindc_70_mod
    use iso_c_binding, only: c_char
    implicit none
contains
    function fortran_explicit() result(Result_Char) bind(c)
        character(kind=c_char) :: Result_Char
        character(len=3) :: local
        local = 'abc'
        Result_Char = local(2:2)
    end function

    function fortran_default() result(c) bind(c)
        character(len=1) :: c
        c = 'd'
    end function

    function fortran_implicit() bind(c)
        character(kind=c_char) :: fortran_implicit
        fortran_implicit = 'i'
    end function

    function fortran_forward() result(c) bind(c)
        interface
            function c_explicit() result(c) bind(c)
                import :: c_char
                character(kind=c_char) :: c
            end function
        end interface
        character(kind=c_char) :: c
        c = c_explicit()
    end function
end module

program bindc_70
    use iso_c_binding, only: c_char, c_int
    use bindc_70_mod
    implicit none
    interface
        function c_explicit() result(c) bind(c)
            import :: c_char
            character(kind=c_char) :: c
        end function
        function c_default() result(c) bind(c)
            character :: c
        end function
        function c_implicit() bind(c)
            import :: c_char
            character(kind=c_char) :: c_implicit
        end function
        character(kind=c_char) function c_prefix() bind(c)
            import :: c_char
        end function
        integer(c_int) function bindc_70_check() bind(c)
            import :: c_int
        end function
    end interface
    character :: c
    character(len=3) :: text

    if (c_explicit() /= 'q') error stop 1
    if (c_default() /= 'r') error stop 2
    if (c_implicit() /= 's') error stop 3
    if (c_prefix() /= 't') error stop 4
    if (fortran_explicit() /= 'b') error stop 5
    if (fortran_default() /= 'd') error stop 6
    if (fortran_implicit() /= 'i') error stop 7
    if (bindc_70_check() /= 0) error stop 8
    if (source_result() /= 'xyz') error stop 9
    if (fortran_forward() /= 'q') error stop 10

    c = c_explicit()
    if (c /= 'q') error stop 11
    c = c_default()
    if (c /= 'r') error stop 12
    c = c_implicit()
    if (c /= 's') error stop 13
    c = c_prefix()
    if (c /= 't') error stop 14
    c = fortran_explicit()
    if (c /= 'b') error stop 15
    c = fortran_default()
    if (c /= 'd') error stop 16
    c = fortran_implicit()
    if (c /= 'i') error stop 17
    c = fortran_forward()
    if (c /= 'q') error stop 18
    text = c_explicit()
    if (text /= 'q  ') error stop 19
    text = source_result()
    if (text /= 'xyz') error stop 20
contains
    function source_result() result(c)
        character(len=3) :: c
        c = 'xyz'
    end function
end program
