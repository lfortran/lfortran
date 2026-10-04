program present_optional_procedure
    implicit none
    call check(expected=.false.)
    call check(value, .true.)
contains
    integer function value()
        value = 3
    end function
    subroutine check(proc, expected)
        interface
            integer function proc()
            end function
        end interface
        optional :: proc
        logical, intent(in) :: expected
        if (present(proc) .neqv. expected) error stop
        if (present(proc)) then
            if (proc() /= 3) error stop
        end if
    end subroutine
end program
