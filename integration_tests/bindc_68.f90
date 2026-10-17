module bindc_68_mod
    implicit none
    character, bind(c) :: x(1)
    character, dimension(1), bind(C, name="mpi_fortran_argv_null") :: a1
    character, dimension(1,1), bind(C, name="mpi_fortran_argvs_null") :: a2
end module

program bindc_68
    use iso_c_binding, only: c_char, c_int
    use bindc_68_mod, only: x, a1, a2
    implicit none
    interface
        character(kind=c_char) function bindc_68_get(which) bind(c)
            import :: c_char, c_int
            integer(c_int), value :: which
        end function
    end interface
    x(1) = 'a'
    a1(1) = 'b'
    a2(1,1) = 'c'
    if (x(1) /= 'a') error stop
    if (a1(1) /= 'b') error stop
    if (a2(1,1) /= 'c') error stop
    if (size(a2) /= 1) error stop
    if (bindc_68_get(0) /= 'a') error stop
    if (bindc_68_get(1) /= 'b') error stop
    if (bindc_68_get(2) /= 'c') error stop
    print *, x(1), a1(1), a2(1,1)
end program
