program subroutines_23
    ! explicit-shape local array whose bound is a host array element (#4648)
    implicit none

    integer :: nang(2) = [1, 2]

    call my_subroutine

contains

    subroutine my_subroutine
        integer, dimension(nang(2)) :: fxhv

        fxhv = [1, 2]

        print *, fxhv
        if (size(fxhv) /= 2) error stop
        if (fxhv(1) /= 1 .or. fxhv(2) /= 2) error stop
        if (sum(fxhv) /= 3) error stop
    end subroutine my_subroutine

end program
