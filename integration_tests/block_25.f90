program block_25
    ! An interface block in a BLOCK construct declares the external
    ! procedures q and s, which are defined after the program.
    implicit none
    integer :: r
    r = 0
    block
        interface
            integer function q(k)
                integer, intent(in) :: k
            end function q

            subroutine s(y)
                integer, intent(inout) :: y
            end subroutine s
        end interface
        r = q(2)
        if (r /= 6) error stop
        call s(r)
        if (r /= 7) error stop
    end block
    print *, r
end program block_25

integer function q(k)
    implicit none
    integer, intent(in) :: k
    q = 3*k
end function q

subroutine s(y)
    implicit none
    integer, intent(inout) :: y
    y = y + 1
end subroutine s
