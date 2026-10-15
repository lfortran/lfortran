program block_22
    ! The external procedures q and s, defined in block_22_procs.f90, are
    ! declared only by an interface block in a BLOCK construct, which hides
    ! the use-associated module procedure q there.
    use block_22_m, only: q
    implicit none
    integer :: r
    if (q(1) /= 1000) error stop
    block
        interface
            integer function q(k)
                integer, intent(in) :: k
            end function q

            subroutine s(y)
                integer, intent(out) :: y
            end subroutine s
        end interface
        if (q(1) /= 3) error stop
        call s(r)
        if (r /= 30) error stop
    end block
    if (q(2) /= 2000) error stop
    print *, q(1), r
end program block_22
