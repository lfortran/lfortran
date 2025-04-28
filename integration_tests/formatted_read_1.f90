program test_stdin
    use iso_c_binding
    implicit none
    character(len=19) :: a
    integer :: istty
    logical :: is_gfortran = .false.

    interface
        function isatty(fd) bind(C)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: isatty
        end function
    end interface

#ifdef __GFORTRAN__
    is_gfortran = .true.
#endif

    istty = isatty(0_c_int)

    if (istty == 1) then
        if (is_gfortran) then
            call execute_command_line("gfortran formatted_read_1.f90 && ./a.out < formatted_read_1.f90")
        else
            call execute_command_line("./src/bin/lfortran --cpp formatted_read_1.f90 < formatted_read_1.f90")
        end if
    else
        ! If not interactive, read from stdin
        read(*, '(a)') a
        print *, "From stdin:", trim(a)
    end if
end program