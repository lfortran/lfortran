program block_21
    ! An interface block in a BLOCK construct declares the external procedures
    ! q and s, and the generic interface g, in the scope of the BLOCK, where
    ! they hide the program's internal procedures of the same names.
    implicit none
    integer :: r
    if (q(1) /= 1000) error stop
    call s(r)
    if (r /= 10) error stop
    block
        interface
            integer function q(k)
                integer, intent(in) :: k
            end function q

            subroutine s(y)
                integer, intent(out) :: y
            end subroutine s
        end interface
        interface g
            procedure :: q
            real function h(x)
                real, intent(in) :: x
            end function h
        end interface g
        if (q(1) /= 3) error stop
        call s(r)
        if (r /= 30) error stop
        if (g(2) /= 6) error stop
        if (abs(g(2.0) - 8.0) > 1e-6) error stop
        block
            if (q(2) /= 6) error stop
        end block
    end block
    if (q(2) /= 2000) error stop
    call s(r)
    if (r /= 10) error stop
    if (work() /= 9) error stop
    print *, q(1), work()
contains
    integer function q(k)
        integer, intent(in) :: k
        q = 1000*k
    end function q

    subroutine s(y)
        integer, intent(out) :: y
        y = 10
    end subroutine s

    integer function work()
        work = 0
        block
            interface
                integer function q(k)
                    integer, intent(in) :: k
                end function q
            end interface
            work = work + q(1)
            block
                work = work + q(2)
            end block
        end block
    end function work
end program block_21

integer function q(k)
    integer, intent(in) :: k
    q = 3*k
end function q

subroutine s(y)
    integer, intent(out) :: y
    y = 30
end subroutine s

real function h(x)
    real, intent(in) :: x
    h = 4*x
end function h
