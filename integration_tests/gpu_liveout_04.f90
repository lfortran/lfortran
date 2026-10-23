! A scalar a `do concurrent` defines in one iteration keeps that value
! (Fortran 2018, 11.1.7.5) when a procedure the program contains reads it
! after the loop, or a GO TO runs the statements before the loop again.
program gpu_liveout_04
    implicit none
    integer :: k, m, i, it
    integer :: a(100)
    k = 0
    do concurrent (i = 1:100)
        a(i) = i
        if (i == 7) k = 7
    end do
    call show()
    m = 0
    it = 0
10  continue
    if (it == 1) then
        print *, m
        if (m /= 9) error stop 2
        stop
    end if
    do concurrent (i = 1:100)
        a(i) = 2 * i
        if (i == 9) m = 9
    end do
    it = it + 1
    goto 10
contains
    subroutine show()
        print *, k
        if (k /= 7) error stop 1
    end subroutine
end program
