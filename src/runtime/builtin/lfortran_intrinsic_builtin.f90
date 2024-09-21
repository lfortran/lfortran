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

end interface

end module
