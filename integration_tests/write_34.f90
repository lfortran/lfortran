program write_34
implicit none
logical :: a, b, c
character(len=100) :: line
integer :: off

a = .true.
b = .false.
c = .true.

! Test list-directed write of logical values:
! - output may start with a leading blank (carriage control); this is
!   processor-dependent, so we accept it as optional
! - logical values should be separated by a single space
write(line, *) a, b, c

off = 0
if (line(1:1) == ' ') off = 1

if (line(1+off:1+off) /= 'T') error stop "expected T"
if (line(2+off:2+off) /= ' ') error stop "expected separator"
if (line(3+off:3+off) /= 'F') error stop "expected F"
if (line(4+off:4+off) /= ' ') error stop "expected separator"
if (line(5+off:5+off) /= 'T') error stop "expected T"

print *, "write_29: PASSED"
end program
