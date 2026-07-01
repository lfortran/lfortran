program namelist_33
    implicit none
    call read_values()
contains
    subroutine read_values()
        integer :: values(2)
        namelist /settings/ values

        open(10, status="scratch")
        write(10, '(A)') "&settings"
        write(10, '(A)') "values = 3, 7"
        write(10, '(A)') "/"
        rewind(10)
        read(10, settings)
        if (any(values /= [3, 7])) error stop
        print *, "test passed"
    end subroutine read_values
end program namelist_33
