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

    if (c_explicit() /= 'q') error stop 1
    if (c_default() /= 'r') error stop 2
    if (c_implicit() /= 's') error stop 3
    if (c_prefix() /= 't') error stop 4
    if (fortran_explicit() /= 'b') error stop 5
    if (fortran_default() /= 'd') error stop 6
    if (fortran_implicit() /= 'i') error stop 7
    if (bindc_70_check() /= 0) error stop 8
    if (source_result() /= 'xyz') error stop 9
contains
    function source_result() result(c)
        character(len=3) :: c
        c = 'xyz'
    end function
end program
