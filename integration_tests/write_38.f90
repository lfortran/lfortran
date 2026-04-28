program write_38
! Test list-directed output field width for real values
implicit none
real :: x
real(8) :: d
character(len=100) :: line

x = 2.5
write(line, *) x
if (len_trim(line) /= 13) error stop
if (line(1:17) /= '   2.50000000    ') error stop

d = 2.5d0
write(line, *) d
if (line(1:26) /= '   2.5000000000000000     ') error stop

! Test zero
x = 0.0
write(line, *) x
if (line(1:17) /= '   0.00000000    ') error stop

! Test E-format (small value)
x = 0.05
write(line, *) x
if (line(1:17) /= '   5.00000007E-02') error stop

print *, "All list-directed real formatting tests passed."
end program
