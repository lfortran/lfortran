module spec_expr_03_mod
    ! A function result in a specification expression is finalized once,
    ! before the executable constructs of the procedure (F2018 7.5.6.3 p6),
    ! however many times the array bounds are used in the body.
    implicit none
    integer :: nfin = 0

    type :: t
        integer :: c = 0
    contains
        final :: fin
    end type

contains

    pure function construct(c) result(r)
        integer, intent(in) :: c
        type(t) :: r
        r%c = c
    end function

    pure integer function comp(self)
        type(t), intent(in) :: self
        comp = self%c
    end function

    subroutine fin(self)
        type(t), intent(inout) :: self
        if (self%c /= 0) nfin = nfin + 1
    end subroutine

end module

program spec_expr_03
    use spec_expr_03_mod, only: construct, comp, nfin
    implicit none

    call s()
    if (nfin /= 1) error stop "s: wrong number of finalizations"

    nfin = 0
    call s_bounds(2)
    if (nfin /= 2) error stop "s_bounds: wrong number of finalizations"
    print *, "ok"

contains

    subroutine s()
        real :: tmp(comp(construct(3)))
        if (nfin /= 1) error stop "s: result not finalized before the body"
        tmp = 1
        if (size(tmp) /= 3) error stop "s: size"
        if (abs(sum(tmp) - 3) > 1e-6) error stop "s: sum"
        if (ubound(tmp, 1) /= 3) error stop "s: ubound"
        if (nfin /= 1) error stop "s: result finalized again in the body"
    end subroutine

    subroutine s_bounds(n)
        integer, intent(in) :: n
        integer :: b(comp(construct(n)):comp(construct(n + 3)))
        if (nfin /= 2) error stop "s_bounds: results not finalized before the body"
        b = 7
        if (lbound(b, 1) /= 2 .or. ubound(b, 1) /= 5) error stop "s_bounds: bounds"
        if (size(b) /= 4) error stop "s_bounds: size"
        if (b(2) /= 7 .or. b(5) /= 7) error stop "s_bounds: values"
        if (nfin /= 2) error stop "s_bounds: results finalized again in the body"
    end subroutine

end program
