program read_74
implicit none

! Test list-directed read of multiple types from a file
! with comma-separated values including D-exponent notation

character(*), parameter :: cdata = "2, 2.5D0, 2.5D0, T, (3.0,4.0), 'TEST'"

integer :: i1
real :: r1
double precision :: d1
logical :: l1
complex :: c1
character(8) :: s1

open (42, file='read_74.dat', status='replace', form='formatted')
write (42,'(a)') cdata
rewind (42)
read (42, *) i1, r1, d1, l1, c1, s1

if (i1 /= 2) error stop
if (abs(r1 - 2.5) > 0.0001) error stop
if (abs(d1 - 2.5d0) > 0.0001d0) error stop
if (.not. l1) error stop
if (abs(real(c1) - 3.0) > 0.0001) error stop
if (abs(aimag(c1) - 4.0) > 0.0001) error stop
if (s1 /= 'TEST') error stop

print *, "PASS"

close (42, status='delete')

end program
