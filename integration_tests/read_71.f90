program read_71
! Test formatted READ into 2D complex array slice with ES format.
! Verifies that the exponent is correctly parsed when reading into
! an array section like arr(1,:).
implicit none
complex :: arr(1, 2)
character(62) :: s

s = "   1.000000E+00    5.000000E-01   2.000000E+00    3.000000E-01"

read(s, '(es15.6,1x,es15.6,1x,es15.6,1x,es15.6)') arr(1, :)

if (abs(real(arr(1,1)) - 1.0) > 1.0e-5) error stop
if (abs(aimag(arr(1,1)) - 0.5) > 1.0e-5) error stop
if (abs(real(arr(1,2)) - 2.0) > 1.0e-5) error stop
if (abs(aimag(arr(1,2)) - 0.3) > 1.0e-5) error stop

print *, "PASS"
end program
