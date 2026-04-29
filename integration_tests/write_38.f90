program write_38
! Test list-directed output for real values in a portable way.
!
! The exact field width and number of fractional digits emitted by
! list-directed WRITE(*,*) for reals is processor-dependent (GFortran/
! LFortran use Gw.dEe-style fixed widths; Flang uses minimal widths).
! The portable, standard-mandated property is that list-directed output
! is itself valid list-directed input, so we round-trip values via an
! internal read and verify them within tolerance.
implicit none
real :: x, y
real(8) :: d, e
character(len=100) :: line

x = 2.5
write(line, *) x
y = 0.0
read(line, *) y
if (abs(y - x) > 1.0e-6) error stop

d = 2.5d0
write(line, *) d
e = 0.0d0
read(line, *) e
if (abs(e - d) > 1.0d-14) error stop

x = 0.0
write(line, *) x
y = 1.0
read(line, *) y
if (y /= 0.0) error stop

x = 0.05
write(line, *) x
y = 0.0
read(line, *) y
if (abs(y - x) > 1.0e-6) error stop

print *, "All list-directed real formatting tests passed."
end program
