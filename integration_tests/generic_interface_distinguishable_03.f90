! With implicit interfaces, a dummy argument that is declared like a data
! object but referenced as a procedure (`real :: f` and `f(a)`, or an
! implicitly typed `f` in `call f(x)`) is a dummy procedure, which is
! distinguishable from a dummy data object (F2018 C1514).
module generic_interface_distinguishable_03_m
    integer :: which = 0

    interface integrate
        module procedure integ_f, integ_v
    end interface

    interface apply
        module procedure apply_s, apply_v
    end interface

contains

    real function integ_f(f, a, b)
        real a, b, f
        integ_f = (b - a) * (f(a) + f(b)) / 2
        which = 1
    end function

    real function integ_v(y, a, b)
        real y, a, b
        integ_v = y * (b - a)
        which = 2
    end function

    subroutine apply_s(s, x)
        real x
        call s(x)
    end subroutine

    subroutine apply_v(y, x)
        real y, x
        which = 4 + 0 * int(y + x)
    end subroutine

    real function sq(x)
        real x
        sq = x * x
    end function

    subroutine mark(x)
        real x
        which = 3 + 0 * int(x)
    end subroutine

end module

program generic_interface_distinguishable_03
    use generic_interface_distinguishable_03_m
    implicit none
    real :: v

    v = integrate(2.0, 0.0, 1.0)
    if (which /= 2) error stop
    if (abs(v - 2.0) > 1e-6) error stop

    v = integrate(sq, 0.0, 2.0)
    if (which /= 1) error stop
    if (abs(v - 4.0) > 1e-6) error stop

    call apply(mark, 1.0)
    if (which /= 3) error stop
    call apply(2.0, 1.0)
    if (which /= 4) error stop

    print *, "ok"
end program
