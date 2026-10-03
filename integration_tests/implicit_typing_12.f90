program implicit_typing_12
    ! Regression test: with --implicit-typing, an array-constructor
    ! implied-do iterator (here `i`) must be local to the implied-do
    ! and must NOT leak into the program scope, otherwise it would be
    ! host-associated in the contained procedures below.
    integer ii
    integer, parameter :: jmin(1:10) = (/ (i, i = 1, 10) /)
    integer, parameter :: kmin(1:10) = (/ (ii, ii = 1, 10) /)
    integer, parameter :: lmin(1:10) = (/ (iii, iii = 1, 10) /)
    integer iii

    if (jmin(1) /= 1 .or. jmin(10) /= 10) error stop
    if (kmin(1) /= 1 .or. kmin(10) /= 10) error stop
    if (lmin(1) /= 1 .or. lmin(10) /= 10) error stop

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

        ! `i` is NOT host-associated (not declared in the program), so
        ! `one` modifies a separate local `i`. `ii` and `iii` ARE
        ! declared at program scope and so are host-associated.
        if (i /= 0) error stop
        if (ii /= 99) error stop
        if (iii /= 999) error stop
    end subroutine

end program
