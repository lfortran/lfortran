! With implicit typing, a name given the INTRINSIC attribute in a module is
! the intrinsic procedure, not an implicitly typed variable, also when an
! access statement names it before or after the INTRINSIC statement.
module implicit_typing_19_mod
intrinsic sin
public :: cos
intrinsic cos
intrinsic sqrt
private :: sqrt
interface
    module real function g_sub(x)
    real, intent(in) :: x
    end function
end interface
contains
real function g(x)
real, intent(in) :: x
g = sin(x)
end function
real function h(x)
real, intent(in) :: x
h = sqrt(x)
end function
end module

submodule (implicit_typing_19_mod) implicit_typing_19_sm
contains
module real function g_sub(x)
real, intent(in) :: x
g_sub = sin(x) + cos(x)
end function
end submodule

program implicit_typing_19
use implicit_typing_19_mod
x = 0.5
if (abs(sin(x) - 0.47942554) > 1e-6) error stop 1
if (abs(cos(x) - 0.87758256) > 1e-6) error stop 2
if (abs(g(x) - 0.47942554) > 1e-6) error stop 3
if (abs(g_sub(x) - 1.3570081) > 1e-6) error stop 4
if (abs(h(16.0) - 4.0) > 1e-6) error stop 5
if (abs(sqrt(9.0) - 3.0) > 1e-6) error stop 6
print *, "ok"
end program
