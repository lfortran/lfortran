module procedure_48_mod
    implicit none

    procedure(stop_error_routine), pointer :: stop_error => null()

    abstract interface
        subroutine stop_error_routine(line, output, msg)
            character(*), intent(in), optional :: line
            integer, intent(in), optional :: output
            character(*), intent(in), optional :: msg
        end subroutine stop_error_routine
    end interface

contains

    subroutine equation_setup_error_routines(stop_error_sub)
        procedure(stop_error_routine) :: stop_error_sub
        stop_error => stop_error_sub
    end subroutine equation_setup_error_routines

    subroutine check_for_error(errmsg, ln)
        character(*), intent(in) :: errmsg
        character(*), intent(in), optional :: ln

        if (associated(stop_error)) then
            call stop_error(line=ln, output=6, msg=errmsg)
        end if
    end subroutine check_for_error

    subroutine impl_stop_error(line, output, msg)
        character(*), intent(in), optional :: line
        integer, intent(in), optional :: output
        character(*), intent(in), optional :: msg

        if (present(line)) then
            if (line /= "LN") error stop
        end if
        if (present(output)) then
            if (output /= 6) error stop
        end if
        if (present(msg)) then
            if (msg /= "ERR") error stop
        end if
    end subroutine impl_stop_error

end module procedure_48_mod

program procedure_48
    use procedure_48_mod
    implicit none

    call equation_setup_error_routines(impl_stop_error)
    call check_for_error("ERR", "LN")
end program procedure_48
