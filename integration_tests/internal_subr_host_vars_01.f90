! Verify host association for internal subroutines and enforce via error stop
subroutine add(x, y)
    implicit none
    real, intent(in) :: x, y
    real :: s

    s = x + y
    if (s /= 3.0) error stop

contains

    subroutine add2()
        implicit none
        real :: t
        ! add2 should be able to see x, y from the host
        t = x + y + 2.0
        if (t /= 5.0) error stop
    end subroutine add2

end subroutine add


program internal_subr_host_vars_01
    implicit none
    call add(1.0, 2.0)
end program internal_subr_host_vars_01
