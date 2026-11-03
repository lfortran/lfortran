! A recursive procedure that passes its dummy procedure to itself (adaptive
! quadrature) and to another procedure that only calls it. With --fast, a
! call that passes it an internal procedure becomes a call of a copy in the
! host, and the recursive call in the copy calls the copy itself.
module procedure_argument_specialization_03_m
implicit none
abstract interface
    real(8) function fn(x)
        real(8), intent(in) :: x
    end function
end interface
contains
real(8) function simpson(f, a, b) result(s)
    procedure(fn) :: f
    real(8), intent(in) :: a, b
    s = (b - a)/6*(f(a) + 4*f((a + b)/2) + f(b))
end function

recursive real(8) function adapt(f, a, b, whole, tol, depth) result(s)
    procedure(fn) :: f
    real(8), intent(in) :: a, b, whole, tol
    integer, intent(in) :: depth
    real(8) :: m, left, right
    m = (a + b)/2
    left = simpson(f, a, m)
    right = simpson(f, m, b)
    if (depth <= 0 .or. abs(left + right - whole) <= 15*tol) then
        s = left + right
    else
        s = adapt(f, a, m, left, tol/2, depth - 1) + &
            adapt(f, m, b, right, tol/2, depth - 1)
    end if
end function

real(8) function integrate(f, a, b) result(s)
    procedure(fn) :: f
    real(8), intent(in) :: a, b
    s = adapt(f, a, b, simpson(f, a, b), 1d-10, 30)
end function
end module

program procedure_argument_specialization_03
use procedure_argument_specialization_03_m, only: integrate
implicit none
real(8) :: k, r
k = 2
r = integrate(g, 0d0, 1d0)
! the integral of exp(2x) over [0, 1]
if (abs(r - (exp(2d0) - 1)/2) > 1d-8) error stop 1
k = 3
r = integrate(g, 0d0, 1d0)
if (abs(r - (exp(3d0) - 1)/3) > 1d-8) error stop 2
print *, r
contains
real(8) function g(x)
    real(8), intent(in) :: x
    g = exp(k*x)
end function
end program
