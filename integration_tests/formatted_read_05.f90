program formatted_read_05
! Test ES and EN format descriptors with exponent width specifier on read
implicit none
real :: a, b
character(len=64) :: line

! Test ES format with e2 exponent width
line = " 1.00000000E+00  3.00000000E+00"
read(line, "(es15.8e2,1x,es15.8e2)") a, b
print *, a, b
if (abs(a - 1.0) > 0.001) error stop
if (abs(b - 3.0) > 0.001) error stop

! Test EN format with e2 exponent width
line = " 1.00000000E+00  5.00000000E+02"
read(line, "(en15.8e2,1x,en15.8e2)") a, b
print *, a, b
if (abs(a - 1.0) > 0.001) error stop
if (abs(b - 500.0) > 1.0) error stop

! Test ES format without exponent width (should still work)
line = " 2.50000000E+01"
read(line, "(es15.8)") a
print *, a
if (abs(a - 25.0) > 0.01) error stop

print *, "PASS"
end program
