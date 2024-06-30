module lfortran_intrinsic_builtin
implicit none

interface
    subroutine move_alloc(from, to)
    integer, allocatable, intent(inout) :: from
    integer, allocatable, intent(out) :: to
    end subroutine

    integer function int(x, kind)
    real, intent(in) :: x(:)
    integer, optional, intent(in) :: kind
    end function

    integer function len(x, kind)
    character(len=*), intent(in) :: x
    integer, optional, intent(in) :: kind
    end function

    logical function present(x)
    integer, optional, intent(in) :: x
    end function

    logical function is_iostat_eor(i) result(r)
    integer, intent(in) :: i
    end function

    logical function is_iostat_end(i) result(r)
    integer, intent(in) :: i
    end function

    subroutine get_command_argument(number, value, length, status)
    integer, intent(in) :: number
    character(len=*), optional, intent(out) :: value
    integer, optional, intent(out) :: length
    integer, optional, intent(out) :: status
        error stop "Not implemented yet"
    end subroutine

    subroutine execute_command_line(command, wait, exitstat, cmdstat, cmdmsg)
    character(len=*), intent(in) :: command
    logical, intent(in), optional :: wait
    integer, intent(in), optional :: exitstat
    integer, intent(in), optional :: cmdstat
    character(len=*), intent(in), optional :: cmdmsg
        error stop "Not implemented yet"
    end subroutine

    subroutine get_environment_variable(name, value, length, status, trim_name)
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: value
    integer, intent(in), optional :: length
    integer, intent(in), optional :: status
    logical, intent(in), optional :: trim_name
        error stop "Not implemented yet"
    end subroutine

    integer function command_argument_count() result(r)
        error stop "Not implemented yet"
    end function

end interface

end module
