program implied_do_loops39
    integer ii
    integer, parameter :: jmin(1:10) = (/ (i, i = 1, 10) /)
    integer, parameter :: kmin(1:10) = (/ (ii, ii = 1, 10) /)
    integer, parameter :: lmin(1:10) = (/ (iii, iii = 1, 10) /)
    integer iii, n

    do n = 1, 10
        if (jmin(n) /= n) error stop 10
        if (kmin(n) /= n) error stop 11
        if (lmin(n) /= n) error stop 12
    end do

    call two

contains

    subroutine one
        i = 99
        ii = 99
        iii = 999
    end subroutine

    subroutine two
        i = 0
        ii = 0
        iii = 0
        call one

        if (i /= 0) error stop 1
        if (ii /= 99) error stop 2
        if (iii /= 999) error stop 3
    end subroutine

end program implied_do_loops39
