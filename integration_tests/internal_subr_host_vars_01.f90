! Verify host association for internal subroutines and enforce via error stop
subroutine add(x, y)
    implicit none
    real, intent(in) :: x, y
    real :: s

    s = x + y
    if (abs(s - (x + y)) > 1.0e-6) error stop "host add: s mismatch"

contains

    subroutine add2()
        implicit none
        real :: t
        ! add2 should be able to see x, y from the host
        t = x + y + 2.0
        if (abs(t - (x + y + 2.0)) > 1.0e-6) error stop "host add2: t mismatch"
    end subroutine add2

end subroutine add


program main
    implicit none
    call add(1.0, 2.0)
end program main
