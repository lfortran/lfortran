program file_57
! Test unformatted stream write/read of a character array
implicit none
character(5) :: a(2), b(2)
integer :: unit, n

a(1) = 'hello'
a(2) = 'world'

! Write character array to unformatted stream file
open(newunit=unit, file='file_57.bin', status='replace', &
     action='write', access='stream')
write(unit) a
inquire(unit, pos=n)
close(unit)

! 2 elements * 5 chars = 10 bytes; file position should be 11 (10+1)
if (n /= 11) error stop

! Read it back and verify
open(newunit=unit, file='file_57.bin', status='old', &
     action='read', access='stream')
read(unit) b
close(unit, status='delete')

if (b(1) /= 'hello') error stop
if (b(2) /= 'world') error stop

print *, "PASS"
end program
