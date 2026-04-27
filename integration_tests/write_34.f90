program write_34
implicit none
logical :: a, b, c
character(len=100) :: line

a = .true.
b = .false.
c = .true.

! Test list-directed write of logical values:
! - output should start with a leading blank (carriage control)
! - logical values should be separated by a single space
write(line, *) a, b, c

if (line(1:1) /= ' ') error stop "missing leading blank"
if (line(2:2) /= 'T') error stop "expected T at position 2"
if (line(3:3) /= ' ') error stop "expected space at position 3"
if (line(4:4) /= 'F') error stop "expected F at position 4"
if (line(5:5) /= ' ') error stop "expected space at position 5"
if (line(6:6) /= 'T') error stop "expected T at position 6"

print *, "write_29: PASSED"
end program
