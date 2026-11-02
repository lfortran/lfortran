! With --implicit-typing, `implicit none` in a main program with internal
! procedures applies to its body and to internal procedures without an
! IMPLICIT statement (F2018 8.7 p4).
program implicit_none_host_01
    implicit none
    y = 1  ! {Error} `y` is not declared
    call s()
contains
    subroutine s()
        q = 1  ! {Error} `q` is not declared
    end subroutine
    subroutine t(n, x)
        integer, intent(in) :: x(n)  ! {Error} `n` is used before it is typed
        integer, intent(in) :: n
    end subroutine
end program
