program test_stdin
    use iso_c_binding
    implicit none
    character(len=100) :: a
    integer :: istty
    logical :: is_gfortran = .false.
    integer :: iostat_value

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
    print *, "istty:", istty  

    if (istty == 1) then
        if (is_gfortran) then
            call execute_command_line("gfortran -cpp formatted_read_1.f90 -o formatted_read_1 && " // &
    "formatted_read_1 < formatted_read_1")
        else
            call execute_command_line("../src/bin/lfortran --cpp formatted_read_1.f90 < formatted_read_1.f90")
        end if
    else

        read(*, '(a)', iostat=iostat_value) a
        if (iostat_value /= 0) then
            print *, "Error reading from stdin. IOSTAT =", iostat_value
            a = ""  
        else
            print *, "From stdin:", trim(a)
        end if
    end if
end program