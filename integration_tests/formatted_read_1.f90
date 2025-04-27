program test_stdin
    use iso_c_binding
    implicit none
    character(len=19) :: a
    integer :: istty

    interface
        function isatty(fd) bind(C)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: isatty
        end function
    end interface

    istty = isatty(0_c_int)

    if (istty == 1) then
        call execute_command_line("./src/bin/lfortran ./integration_tests/formatted_read_1.f90 " // &
                          "< ./tesintegration_teststs/formatted_read_1.f90")
    else
        ! If not interactive, just read from stdin as usual
        read(*, '(a)') a
        print *, "From stdin:", trim(a)
    end if
end program