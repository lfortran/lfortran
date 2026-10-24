! Procedures of one name in different scopes, each reached from device
! code, have to stay distinct procedures there.
module gpu_metal_579_a
implicit none
contains
pure real function e(x)
    real, intent(in) :: x
    e = x + 1
end function

pure real function k(x)
    real, intent(in) :: x
    k = x + 5
end function

pure real function sq(x)
    real, intent(in) :: x
    sq = x + 2
end function

pure real function apply(g, x)
    interface
        pure real function g(x)
            real, intent(in) :: x
        end function
    end interface
    real, intent(in) :: x
    apply = g(x)
end function

! Calls the module procedure e.
pure real function h(x)
    real, intent(in) :: x
    h = e(x)
end function

! Calls its own e, which calls k, which nothing else calls.
pure real function f(x)
    real, intent(in) :: x
    f = e(x)
contains
    pure real function e(x)
        real, intent(in) :: x
        e = 10 * k(x)
    end function
end function

! Hands its own sq to apply; s calls the module procedure sq.
pure real function r(x)
    real, intent(in) :: x
    r = apply(sq, x)
contains
    pure real function sq(x)
        real, intent(in) :: x
        sq = x * x
    end function
end function

pure real function s(x)
    real, intent(in) :: x
    s = sq(x)
end function
end module

! A procedure named like one of the module above.
module gpu_metal_579_b
use gpu_metal_579_a, only: apply
implicit none
contains
pure real function e(x)
    real, intent(in) :: x
    e = 3 * x
end function

pure real function hb(x)
    real, intent(in) :: x
    hb = e(x)
end function

! Hands this module's e, the one of the two named e that is renamed in
! device code, to apply.
pure real function pb(x)
    real, intent(in) :: x
    pb = apply(e, x)
end function
end module

program gpu_metal_579
use gpu_metal_579_a, only: h, f, r, s
use gpu_metal_579_b, only: hb, pb
implicit none
integer, parameter :: n = 1000
real :: o(n), p(n), q(n), t(n), u(n), v(n)
integer :: i
do concurrent (i = 1:n)
    o(i) = f(real(i))
    p(i) = h(real(i))
    q(i) = r(real(i))
    t(i) = s(real(i))
    u(i) = hb(real(i))
    v(i) = pb(real(i))
end do
print *, o(1:3)
print *, p(1:3)
print *, q(1:3)
print *, t(1:3)
print *, u(1:3)
print *, v(1:3)
do i = 1, n
    if (abs(o(i) - 10*(real(i) + 5)) > 1e-4*real(i)) error stop 1
    if (abs(p(i) - (real(i) + 1)) > 1e-4*real(i)) error stop 2
    if (abs(q(i) - real(i)**2) > 1e-4*real(i)**2) error stop 3
    if (abs(t(i) - (real(i) + 2)) > 1e-4*real(i)) error stop 4
    if (abs(u(i) - 3*real(i)) > 1e-4*real(i)) error stop 5
    if (abs(v(i) - 3*real(i)) > 1e-4*real(i)) error stop 6
end do
end program
