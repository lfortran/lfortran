module complex_35_mod
implicit none
complex :: x(2) = [(1.0, 2.0), (3.0, 4.0)]
complex(8) :: y(3) = [(1.0d0, 2.0d0), (3.0d0, 4.0d0), (5.0d0, 6.0d0)]
end module

program complex_35
use complex_35_mod
implicit none

if (abs(real(x(1)) - 1.0) > 1e-6) error stop
if (abs(aimag(x(1)) - 2.0) > 1e-6) error stop
if (abs(real(x(2)) - 3.0) > 1e-6) error stop
if (abs(aimag(x(2)) - 4.0) > 1e-6) error stop

if (abs(real(y(1)) - 1.0d0) > 1d-12) error stop
if (abs(aimag(y(1)) - 2.0d0) > 1d-12) error stop
if (abs(real(y(2)) - 3.0d0) > 1d-12) error stop
if (abs(aimag(y(2)) - 4.0d0) > 1d-12) error stop
if (abs(real(y(3)) - 5.0d0) > 1d-12) error stop
if (abs(aimag(y(3)) - 6.0d0) > 1d-12) error stop

print *, "ok"
end program
