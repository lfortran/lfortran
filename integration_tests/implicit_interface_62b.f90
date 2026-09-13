! Companion definitions for implicit_interface_62.

module implicit_interface_62_mod
    implicit none
contains

    ! Unrelated to the external `report_len` below: only the name is shared.
    subroutine report_len(x, y)
        integer, intent(in) :: x
        integer, intent(out) :: y
        y = 2 * x
    end subroutine report_len

    subroutine apply_op(op, k)
        interface
            subroutine op(x, y)
                integer, intent(in) :: x
                integer, intent(out) :: y
            end subroutine op
        end interface
        integer, intent(out) :: k
        call op(7, k)
    end subroutine apply_op

    subroutine run(k)
        integer, intent(out) :: k
        ! Takes the address of the MODULE procedure `report_len`.
        call apply_op(report_len, k)
    end subroutine run

end module implicit_interface_62_mod

! Keeps the module out of the main program's translation unit.
subroutine run_helper(k)
    use implicit_interface_62_mod, only: run
    implicit none
    integer, intent(out) :: k
    call run(k)
end subroutine run_helper

subroutine report_len(msg, n)
    implicit none
    character(len=*), intent(in) :: msg
    integer, intent(out) :: n
    n = len(msg)
end subroutine report_len
