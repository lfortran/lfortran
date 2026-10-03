program template_operator_scope_01
    implicit none
    template tm {sub}
        deferred interface
            elemental real function sub(x, y)
                real, intent(in) :: x, y
            end function
        end interface
    end template
    instantiate tm {operator(-)}
    real :: a(2), b(2)
    character(100) :: line

    a = [3.0, 5.0]
    print *, a - 1.0
    write(line, *) a - 1.0
    call check_output(line)
    write(line, *) a - [1.0, 1.0]
    call check_output(line)
    write(line, *) [3.0, 5.0] - 1.0
    call check_output(line)
    b = a - 1.0
    if (b(1) /= 2.0 .or. b(2) /= 4.0) error stop
    if (any(a - 1.0 /= [2.0, 4.0])) error stop
    if (a(1) - 1.0 /= 2.0) error stop
    call check_host_scope()
contains
    subroutine check_output(text)
        character(*), intent(in) :: text
        real :: values(2)
        integer :: status

        ! A successful write must contain both elements, not just the first.
        read(text, *, iostat=status) values
        if (status /= 0) error stop
        if (values(1) /= 2.0 .or. values(2) /= 4.0) error stop
    end subroutine

    subroutine check_host_scope()
        character(100) :: text
        write(text, *) a - 1.0
        call check_output(text)
    end subroutine
end program
