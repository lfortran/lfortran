program write_38
! Test list-directed output field width for real values.
!
! The leading record blank is processor-dependent, so we strip any
! leading blanks via adjustl and check the remaining content (which
! includes the field's internal/trailing pad).
implicit none
real :: x
real(8) :: d
character(len=100) :: line, t

x = 2.5
write(line, *) x
t = adjustl(line)
if (t(1:14) /= '2.50000000    ') error stop

d = 2.5d0
write(line, *) d
t = adjustl(line)
if (t(1:23) /= '2.5000000000000000     ') error stop

! Test zero
x = 0.0
write(line, *) x
t = adjustl(line)
if (t(1:14) /= '0.00000000    ') error stop

! Test E-format (small value)
x = 0.05
write(line, *) x
t = adjustl(line)
if (t(1:14) /= '5.00000007E-02') error stop

print *, "All list-directed real formatting tests passed."
end program
