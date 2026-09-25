! An `optional` statement preceding a declaration applies only to the
! procedure in which it appears. It must not make a same-named required
! dummy procedure optional in a later procedure or in the host of an
! interface body.
module optional_16_mod
    implicit none
    integer :: ncalls = 0
contains

    subroutine a(p)
        optional :: p
        integer :: p
        if (present(p)) ncalls = ncalls + 100
    end subroutine

    ! `p` is a required dummy procedure; the `optional :: p` in `a`
    ! must not leak into it.
    subroutine f(p)
        interface
            subroutine p(x)
                implicit none
                integer, intent(in) :: x
            end subroutine
        end interface
        call p(1)
    end subroutine

    ! The interface body of `q` declares its own optional dummy `p`; that
    ! must not make the host's required dummy procedure `p` optional.
    subroutine g(q, p)
        interface
            subroutine q(p)
                implicit none
                optional :: p
                integer :: p
            end subroutine
            subroutine p(x)
                implicit none
                integer, intent(in) :: x
            end subroutine
        end interface
        call q()
        call q(5)
        call p(1)
    end subroutine

    subroutine qq(p)
        integer, optional :: p
        if (present(p)) then
            if (p /= 5) error stop
            ncalls = ncalls + 10
        end if
    end subroutine

    subroutine s(x)
        integer, intent(in) :: x
        if (x /= 1) error stop
        ncalls = ncalls + 1
    end subroutine

end module

program optional_16
    use optional_16_mod
    implicit none
    call a()
    if (ncalls /= 0) error stop
    call f(s)
    if (ncalls /= 1) error stop
    call g(qq, s)
    if (ncalls /= 12) error stop
    print *, "ok"
end program
