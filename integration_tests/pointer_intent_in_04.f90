! Three optional arrays forwarded to optional pointer, intent(in) dummies:
! the call is split once on the presence of each, giving 2**3 calls.
module pointer_intent_in_04_mod
    implicit none
contains
    subroutine count_present(p, q, r, n)
        integer, pointer, optional, intent(in) :: p(:), q(:), r(:)
        integer, intent(out) :: n
        n = 0
        if (present(p)) n = n + size(p)
        if (present(q)) n = n + 10*size(q)
        if (present(r)) n = n + 100*size(r)
    end subroutine

    subroutine forward(x, y, z, n)
        integer, optional, target, intent(in) :: x(:), y(:), z(:)
        integer, intent(out) :: n
        call count_present(x, y, z, n)
    end subroutine
end module

program pointer_intent_in_04
    use pointer_intent_in_04_mod
    implicit none
    integer, target :: a(1), b(2), c(3)
    integer :: n
    a = 1
    b = 2
    c = 3
    call forward(n=n)
    if (n /= 0) error stop
    call forward(a, n=n)
    if (n /= 1) error stop
    call forward(y=b, n=n)
    if (n /= 20) error stop
    call forward(z=c, n=n)
    if (n /= 300) error stop
    call forward(a, b, n=n)
    if (n /= 21) error stop
    call forward(a, z=c, n=n)
    if (n /= 301) error stop
    call forward(y=b, z=c, n=n)
    if (n /= 320) error stop
    call forward(a, b, c, n)
    if (n /= 321) error stop
    print *, n
end program
