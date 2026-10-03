! Companion definitions for implicit_interface_61.

subroutine report_len(msg, n)
    implicit none
    character(len=*), intent(in) :: msg
    integer, intent(out) :: n
    n = len(msg)
end subroutine report_len

! Unrelated to the above: its dummy procedure merely happens to be spelled
! `report_len` too. A dummy procedure name is local to this subprogram and must
! not affect the external `report_len` defined in this same file.
subroutine apply_op(report_len, k)
    implicit none
    interface
        subroutine report_len(x, y)
            integer, intent(in) :: x
            integer, intent(out) :: y
        end subroutine report_len
    end interface
    integer, intent(out) :: k
    call report_len(7, k)
end subroutine apply_op

subroutine double_it(x, y)
    implicit none
    integer, intent(in) :: x
    integer, intent(out) :: y
    y = 2 * x
end subroutine double_it
