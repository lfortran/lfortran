! A NON_RECURSIVE host has one instance at a time, so the procedure value
! of its internal procedure needs one thunk, not a pool of
! --internal-procedure-thunks of them.
module host_assoc_24_m
implicit none
abstract interface
    real function integrand(x)
        real, intent(in) :: x
    end function
end interface
contains
real function integrate(f, a, b, n)
    procedure(integrand) :: f
    real, intent(in) :: a, b
    integer, intent(in) :: n
    integer :: i
    real :: h
    h = (b - a)/n
    integrate = 0
    do i = 0, n - 1
        integrate = integrate + h*f(a + (i + 0.5)*h)
    end do
end function

non_recursive function scaled_integral(c) result(r)
    real, intent(in) :: c
    real :: r
    r = integrate(scaled, 0.0, 1.0, 1000)
contains
    real function scaled(x)
        real, intent(in) :: x
        scaled = c*x
    end function
end function
end module

program host_assoc_24
use host_assoc_24_m
implicit none
real :: r
r = scaled_integral(4.0)
print *, r
if (abs(r - 2.0) > 1e-4) error stop 1
end program
