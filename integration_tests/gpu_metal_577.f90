module gpu_metal_577_m
implicit none
abstract interface
    pure real function iface(x)
        real, intent(in) :: x
    end function
end interface
contains

pure real function apply_m(g, x)
    interface
        pure real function g(x)
            real, intent(in) :: x
        end function
    end interface
    real, intent(in) :: x
    apply_m = g(x)
end function

! A procedure contained in the device procedure passed to a module
! procedure.
pure real function w(x)
    real, intent(in) :: x
    w = apply_m(cube, x)
contains
    pure real function cube(x)
        real, intent(in) :: x
        cube = x * x * x
    end function
end function

! A procedure with a dummy procedure that contains a procedure reading a
! dummy of its host by host association.
pure real function apply_h(g, x)
    interface
        pure real function g(x)
            real, intent(in) :: x
        end function
    end interface
    real, intent(in) :: x
    apply_h = g(x) + inner()
contains
    pure real function inner()
        inner = 2 * x
    end function
end function

pure real function v(x)
    real, intent(in) :: x
    v = apply_h(plus3, x)
contains
    pure real function plus3(x)
        real, intent(in) :: x
        plus3 = x + 3
    end function
end function

! Dummy procedures declared with procedure(iface), one of them optional
! and present.
pure real function z(x)
    real, intent(in) :: x
    z = apply_p(thrice, x) + apply_o(x, thrice)
contains
    pure real function apply_p(g, x)
        procedure(iface) :: g
        real, intent(in) :: x
        apply_p = g(x)
    end function
    pure real function apply_o(x, g)
        real, intent(in) :: x
        procedure(iface), optional :: g
        if (present(g)) then
            apply_o = g(x) + 1
        else
            apply_o = x
        end if
    end function
    pure real function thrice(x)
        real, intent(in) :: x
        thrice = 3 * x
    end function
end function

! The issue's scenario: a procedure dummy of a procedure contained in the
! device procedure, passed a procedure contained there too.
pure real function f(x)
    real, intent(in) :: x
    f = apply(sq, x)
contains
    pure real function apply(g, x)
        interface
            pure real function g(x)
                real, intent(in) :: x
            end function
        end interface
        real, intent(in) :: x
        apply = g(x)
    end function
    pure real function sq(x)
        real, intent(in) :: x
        sq = x * x
    end function
end function

! A function dummy passed two different procedures, and a subroutine
! dummy.
pure integer function u(k)
    integer, intent(in) :: k
    integer :: r
    call run(twice, k, r)
    u = r + apply(inc, k) + apply(neg, k)
contains
    pure integer function apply(g, k)
        interface
            pure integer function g(k)
                integer, intent(in) :: k
            end function
        end interface
        integer, intent(in) :: k
        apply = g(k)
    end function
    pure subroutine run(s, k, r)
        interface
            pure subroutine s(k, r)
                integer, intent(in) :: k
                integer, intent(out) :: r
            end subroutine
        end interface
        integer, intent(in) :: k
        integer, intent(out) :: r
        call s(k, r)
    end subroutine
    pure integer function inc(k)
        integer, intent(in) :: k
        inc = k + 1
    end function
    pure integer function neg(k)
        integer, intent(in) :: k
        neg = -2*k
    end function
    pure subroutine twice(k, r)
        integer, intent(in) :: k
        integer, intent(out) :: r
        r = 2*k
    end subroutine
end function
end module

program gpu_metal_577
use gpu_metal_577_m, only: f, u, w, v, z
implicit none
integer, parameter :: n = 10000
real :: o(n), q(n), s(n), t(n)
integer :: p(n), i
do concurrent (i = 1:n)
    o(i) = f(real(i))
    p(i) = u(i)
    q(i) = w(real(i))
    s(i) = v(real(i))
    t(i) = z(real(i))
end do
print *, o(1:4)
print *, p(1:4)
print *, q(1:4)
print *, s(1:4)
print *, t(1:4)
do i = 1, n
    if (abs(o(i) - real(i)**2) > 1e-3*real(i)**2) error stop 1
    if (p(i) /= i + 1) error stop 2
    if (abs(q(i) - real(i)**3) > 1e-3*real(i)**3) error stop 3
    if (abs(s(i) - (3*real(i) + 3)) > 1e-3*real(i)) error stop 4
    if (abs(t(i) - (6*real(i) + 1)) > 1e-3*real(i)) error stop 5
end do
end program
