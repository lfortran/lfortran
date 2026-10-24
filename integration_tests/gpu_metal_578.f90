module gpu_metal_578_m
implicit none
abstract interface
    pure real function iface(x)
        real, intent(in) :: x
    end function
end interface
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

pure real function cube(x)
    real, intent(in) :: x
    cube = x * x * x
end function

! The issue's scenario: a module procedure passed to a module procedure in
! a procedure the loop calls.
pure real function f(x)
    real, intent(in) :: x
    f = apply(sq, x)
end function

! A dummy procedure declared with procedure(iface).
pure real function apply_p(g, x)
    procedure(iface) :: g
    real, intent(in) :: x
    apply_p = g(x)
end function

! A dummy procedure handed on to another procedure.
pure real function twice(g, x)
    interface
        pure real function g(x)
            real, intent(in) :: x
        end function
    end interface
    real, intent(in) :: x
    twice = apply(g, x) + apply(g, x)
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

pure subroutine triple(k, r)
    integer, intent(in) :: k
    integer, intent(out) :: r
    r = 3*k
end subroutine
end module

program gpu_metal_578
use gpu_metal_578_m, only: apply, apply_p, sq, cube, f, twice, run, triple
implicit none
integer, parameter :: n = 1000
real :: o(n), p(n), q(n), s(n)
integer :: r(n), i
do concurrent (i = 1:n)
    ! The issue's scenarios: a module procedure passed in the loop body,
    ! and in a procedure the loop calls.
    o(i) = apply(sq, real(i)) + f(real(i))
    p(i) = twice(cube, real(i))
    call run(triple, i, r(i))
    q(i) = apply(cube, real(i)) - apply(sq, real(i))
    s(i) = apply_p(sq, real(i))
end do
print *, o(1:4)
print *, p(1:4)
print *, r(1:4)
print *, q(1:4)
print *, s(1:4)
do i = 1, n
    if (abs(o(i) - 2*real(i)**2) > 1e-5*real(i)**2) error stop 1
    if (abs(p(i) - 2*real(i)**3) > 1e-5*real(i)**3) error stop 2
    if (r(i) /= 3*i) error stop 3
    if (abs(q(i) - (real(i)**3 - real(i)**2)) > 1e-5*real(i)**3) error stop 4
    if (abs(s(i) - real(i)**2) > 1e-5*real(i)**2) error stop 5
end do
end program
