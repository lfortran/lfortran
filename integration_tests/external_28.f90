program external_28
    ! The program's internal procedures q and s share their names with the
    ! external procedures q and s in external_28_procs.f90, which is compiled
    ! separately; each name must refer to its own body.
    implicit none
    interface
        integer function use_q()
        end function use_q

        subroutine use_s(y)
            integer, intent(out) :: y
        end subroutine use_s
    end interface
    integer :: x
    if (q() /= 1) error stop
    call s(x)
    if (x /= 10) error stop
    if (use_q() /= 2) error stop
    call use_s(x)
    if (x /= 20) error stop
    print *, q(), use_q()
contains
    integer function q()
        q = 1
    end function q

    subroutine s(y)
        integer, intent(out) :: y
        y = 10
    end subroutine s
end program external_28
