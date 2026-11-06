module procedure_argument_specialization_03_m
implicit none
interface
    real(8) function fn(x)
        real(8), intent(in) :: x
    end function fn
end interface

contains

real(8) function adapt(f, a, b, whole, tol, depth) result(s)
    real(8), intent(in) :: a
    real(8), intent(in) :: b
    integer(4), intent(in) :: depth
    procedure(fn) :: f
    real(8) :: left
    real(8) :: m
    real(8) :: right
    real(8), intent(in) :: tol
    real(8), intent(in) :: whole
    m = (a + b)/real(2, kind=8)
    left = simpson(f, a, m)
    right = simpson(f, m, b)
    if (depth <= 0 .or. Abs(left + right - whole) <= real(15, kind=8)*tol) then
        s = left + right
    else
        s = adapt(f, a, m, left, tol/real(2, kind=8), depth - 1) + adapt(f, m, b, right, tol/real(2, kind=8), depth - 1)
    end if
end function adapt

real(8) function integrate(f, a, b) result(s)
    real(8), intent(in) :: a
    real(8), intent(in) :: b
    procedure(fn) :: f
    real(8) :: lfortran_tmp
    lfortran_tmp = simpson(f, a, b)
    s = adapt(f, a, b, lfortran_tmp, 1.0000000000000000e-10_8, 30)
end function integrate

real(8) function simpson(f, a, b) result(s)
    real(8), intent(in) :: a
    real(8), intent(in) :: b
    procedure(fn) :: f
    s = (b - a)/real(6, kind=8)*(f(a) + real(4, kind=8)*f((a + b)/real(2, kind=8)) + f(b))
end function simpson

end module procedure_argument_specialization_03_m

program procedure_argument_specialization_03
use procedure_argument_specialization_03_m, only: integrate
implicit none
real(8) :: k
real(8) :: r
k = real(2, kind=8)
r = integrate__g(0.0000000000000000e+00_8, 1.0000000000000000e+00_8)
if (Abs(r - (Exp(2.0000000000000000e+00_8) - real(1, kind=8))/real(2, kind=8)) > 1.0000000000000000e-08_8) then
    error stop
end if
k = real(3, kind=8)
r = integrate__g(0.0000000000000000e+00_8, 1.0000000000000000e+00_8)
if (Abs(r - (Exp(3.0000000000000000e+00_8) - real(1, kind=8))/real(3, kind=8)) > 1.0000000000000000e-08_8) then
    error stop
end if
print *, r

contains

real(8) function adapt__g(a, b, whole, tol, depth) result(s)
    use procedure_argument_specialization_03_m, only: adapt
    use procedure_argument_specialization_03_m, only: simpson
    real(8), intent(in) :: a
    real(8), intent(in) :: b
    integer(4), intent(in) :: depth
    real(8) :: left
    real(8) :: m
    real(8) :: right
    real(8), intent(in) :: tol
    real(8), intent(in) :: whole
    m = (a + b)/real(2, kind=8)
    left = simpson__g(a, m)
    right = simpson__g(m, b)
    if (depth <= 0 .or. Abs(left + right - whole) <= real(15, kind=8)*tol) then
        s = left + right
    else
        s = adapt__g(a, m, left, tol/real(2, kind=8), depth - 1) + adapt__g(m, b, right, tol/real(2, kind=8), depth - 1)
    end if
end function adapt__g

real(8) function g(x)
    real(8), intent(in) :: x
    g = Exp(k*x)
end function g

real(8) function integrate__g(a, b) result(s)
    use procedure_argument_specialization_03_m, only: adapt
    use procedure_argument_specialization_03_m, only: simpson
    real(8), intent(in) :: a
    real(8), intent(in) :: b
    real(8) :: lfortran_tmp
    lfortran_tmp = simpson__g(a, b)
    s = adapt__g(a, b, lfortran_tmp, 1.0000000000000000e-10_8, 30)
end function integrate__g

real(8) function simpson__g(a, b) result(s)
    real(8), intent(in) :: a
    real(8), intent(in) :: b
    s = (b - a)/real(6, kind=8)*(g(a) + real(4, kind=8)*g((a + b)/real(2, kind=8)) + g(b))
end function simpson__g

end program procedure_argument_specialization_03
