! Optional dummy procedures declared with EXTERNAL, with the OPTIONAL
! attribute given in every order relative to EXTERNAL and the type.
module external_26_mod
    implicit none
contains
    subroutine s1(x, p)
        integer, intent(out) :: x
        integer, external, optional :: p
        if (present(p)) then
            x = p()
        else
            x = 1
        end if
    end subroutine

    subroutine s2(x, p)
        integer, intent(out) :: x
        integer, optional :: p
        external :: p
        if (present(p)) then
            x = p()
        else
            x = 2
        end if
    end subroutine

    subroutine s3(x, p)
        integer, intent(out) :: x
        optional :: p
        external :: p
        integer :: p
        if (present(p)) then
            x = p()
        else
            x = 3
        end if
    end subroutine

    subroutine s4(x, p)
        integer, intent(out) :: x
        external :: p
        optional :: p
        integer :: p
        if (present(p)) then
            x = p()
        else
            x = 4
        end if
    end subroutine

    subroutine s5(x, q)
        integer, intent(inout) :: x
        optional :: q
        external :: q
        if (present(q)) then
            call q(x)
        else
            x = -1
        end if
    end subroutine
end module

integer function five()
    five = 5
end function

subroutine incr(y)
    integer, intent(inout) :: y
    y = y + 1
end subroutine

program external_26
    use external_26_mod
    implicit none
    integer, external :: five
    external :: incr
    integer :: x

    call s1(x)
    if (x /= 1) error stop
    call s1(x, five)
    if (x /= 5) error stop

    call s2(x)
    if (x /= 2) error stop
    call s2(x, five)
    if (x /= 5) error stop

    call s3(x)
    if (x /= 3) error stop
    call s3(x, five)
    if (x /= 5) error stop

    call s4(x)
    if (x /= 4) error stop
    call s4(x, five)
    if (x /= 5) error stop

    x = 0
    call s5(x)
    if (x /= -1) error stop
    x = 3
    call s5(x, incr)
    if (x /= 4) error stop
end program
