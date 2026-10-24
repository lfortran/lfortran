module subroutines_23_mod
    ! A parenthesized actual argument `(x)` is an expression, so the dummy is
    ! associated with a copy of its value, not with `x` itself.
    implicit none
    type :: t
        integer :: c
    contains
        procedure :: tb
    end type
    interface g
        module procedure g_int, g_real
    end interface
contains
    subroutine tb(self, v, w)
        class(t), intent(in) :: self
        integer, intent(in) :: v
        integer, intent(inout) :: w
        w = 5
        if (v /= 1) error stop
    end subroutine

    subroutine g_int(v, w)
        integer, intent(in) :: v
        integer, intent(inout) :: w
        w = 5
        if (v /= 1) error stop
    end subroutine

    subroutine g_real(v, w)
        real, intent(in) :: v
        real, intent(inout) :: w
        w = 5
        if (v /= 1) error stop
    end subroutine

    elemental subroutine el(v, w)
        integer, intent(in) :: v
        integer, intent(inout) :: w
        w = 4 + v
    end subroutine
end module

program subroutines_23
    use subroutines_23_mod
    implicit none
    integer :: x, i, a(3), m(2, 3)
    integer, allocatable :: b(:)
    integer, target :: tgt, ta(3)
    integer, pointer :: p, pa(:)
    type(t) :: d

    x = 1
    call s2((x), x)
    if (x /= 5) error stop

    a = 1
    call s2((a(2)), a(2))
    if (any(a /= [1, 5, 1])) error stop

    d%c = 1
    call s2((d%c), d%c)
    if (d%c /= 5) error stop

    x = 1
    call s2(w=x, v=(x))
    if (x /= 5) error stop

    x = 1
    call s2(((x)), x)
    if (x /= 5) error stop

    tgt = 1
    p => tgt
    call s2((p), tgt)
    if (tgt /= 5) error stop

    a = 1
    do i = 1, 3
        call s2((a(i)), a(i))
    end do
    if (any(a /= 5)) error stop

    a = 1
    call s3((a), a)
    if (any(a /= 5)) error stop

    a = 1
    call s4((a), a)
    if (any(a /= 5)) error stop

    a = 1
    call s3((a(2:3)), a(2:3))
    if (any(a /= [1, 5, 5])) error stop

    m = 1
    call s3((m(1, :)), m(1, :))
    if (any(m(1, :) /= 5) .or. any(m(2, :) /= 1)) error stop

    m = 1
    call s5((m), m)
    if (any(m /= 5)) error stop

    allocate(b(4))
    b = 1
    call s3((b), b)
    if (any(b /= 5)) error stop

    ta = 1
    pa => ta
    call s3((pa), pa)
    if (any(ta /= 5)) error stop

    a = 1
    call s6(a)
    if (any(a /= 5)) error stop

    x = 1
    d%c = 1
    call d%tb((x), x)
    if (x /= 5) error stop

    x = 1
    call g((x), x)
    if (x /= 5) error stop

    x = 1
    call g(w=x, v=(x))
    if (x /= 5) error stop

    a = 1
    call el((a), a)
    if (any(a /= 5)) error stop
contains
    subroutine s2(v, w)
        integer, intent(in) :: v
        integer, intent(inout) :: w
        w = 5
        if (v /= 1) error stop
    end subroutine

    subroutine s3(v, w)
        integer, intent(in) :: v(:)
        integer, intent(inout) :: w(:)
        w = 5
        if (any(v /= 1)) error stop
    end subroutine

    subroutine s4(v, w)
        integer, intent(in) :: v(3)
        integer, intent(inout) :: w(3)
        w = 5
        if (any(v /= 1)) error stop
    end subroutine

    subroutine s5(v, w)
        integer, intent(in) :: v(:, :)
        integer, intent(inout) :: w(:, :)
        w = 5
        if (any(v /= 1)) error stop
    end subroutine

    subroutine s6(y)
        integer, intent(inout) :: y(:)
        call s3((y), y)
    end subroutine
end program
