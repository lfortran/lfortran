module entry_27_mod
    implicit none
contains
    integer function bump(j)
        integer, intent(inout) :: j
        j = j + 1
        bump = j
    end function

    integer function ident(j)
        integer, intent(in) :: j
        ident = j
    end function

    subroutine alt(k, *, *)
        integer, intent(in) :: k
        if (k > 100) return 1
        if (k < 0) return 2
    end subroutine

    function f(n) result(r)
        integer, intent(in) :: n
        integer :: r, j
        j = n
        r = ident(bump(j)) * 2
        return
    entry g(n) result(r)
        j = n
        r = ident(bump(j)) * 3
    end function
end module

subroutine entry_27_s(k, mode)
    use entry_27_mod
    implicit none
    integer :: k, j, mode
    j = 5
    if (mode == 1) goto 20
    k = ident(bump(j))
    return
20  j = 10
    k = ident(bump(j)) + 100
    return
    entry entry_27_e(k, mode)
    j = 40
    k = ident(bump(j))
    if (mode == 2) then
        k = 300
        call alt(k, *30, *40)
        k = -7
    end if
    return
30  k = 2
    return
40  k = 3
end subroutine

subroutine entry_27_t(k)
    use entry_27_mod
    implicit none
    integer :: k
    k = 200
    call alt(k, *10, *20)
    k = -5
    return
10  k = 1
    return
20  k = 2
    return
    entry entry_27_u(k)
    k = -1
    call alt(k, *30, *40)
    k = -5
    return
30  k = 3
    return
40  k = 4
end subroutine

program entry_27
    use entry_27_mod, only: f, g
    implicit none
    integer :: k
    call entry_27_s(k, 0)
    print *, k
    if (k /= 6) error stop
    call entry_27_s(k, 1)
    print *, k
    if (k /= 111) error stop
    call entry_27_e(k, 0)
    print *, k
    if (k /= 41) error stop
    call entry_27_e(k, 2)
    print *, k
    if (k /= 2) error stop
    k = 0
    call entry_27_t(k)
    print *, k
    if (k /= 1) error stop
    call entry_27_u(k)
    print *, k
    if (k /= 4) error stop
    print *, f(4), g(4)
    if (f(4) /= 10) error stop
    if (g(4) /= 15) error stop
end program
