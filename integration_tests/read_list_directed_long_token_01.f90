program read_list_directed_long_token_01
! A list-directed READ of a numeric value that is too long for the reader
! fails or succeeds, but it must consume the whole value, so that the next
! READ starts at the next record. A long real value whose digits beyond the
! reader's limit do not matter is read.
implicit none
integer :: i, u, ios
integer(8) :: i8
real :: x
double precision :: d
character(len=250) :: s
character(len=153) :: z

s = repeat('1', len(s))
z = '0.5' // repeat('0', 150)
open(newunit=u, file='read_list_directed_long_token_01.dat', status='replace')
write(u, '(a)') s
write(u, '(a)') '7'
write(u, '(a)') s
write(u, '(a)') '8'
write(u, '(a)') s
write(u, '(a)') '2.5'
write(u, '(a)') s
write(u, '(a)') '3.5'
write(u, '(a)') z
write(u, '(a)') '4.5'
write(u, '(a)') z
write(u, '(a)') '5.5'
rewind(u)

read(u, *, iostat=ios) i
i = -9
read(u, *, iostat=ios) i
if (ios /= 0 .or. i /= 7) error stop 1
read(u, *, iostat=ios) i8
i8 = -9
read(u, *, iostat=ios) i8
if (ios /= 0 .or. i8 /= 8) error stop 2
read(u, *, iostat=ios) x
x = -9
read(u, *, iostat=ios) x
if (ios /= 0 .or. x /= 2.5) error stop 3
read(u, *, iostat=ios) d
d = -9
read(u, *, iostat=ios) d
if (ios /= 0 .or. d /= 3.5d0) error stop 4
x = -9
read(u, *, iostat=ios) x
if (ios /= 0 .or. x /= 0.5) error stop 5
x = -9
read(u, *, iostat=ios) x
if (ios /= 0 .or. x /= 4.5) error stop 6
d = -9
read(u, *, iostat=ios) d
if (ios /= 0 .or. d /= 0.5d0) error stop 7
d = -9
read(u, *, iostat=ios) d
if (ios /= 0 .or. d /= 5.5d0) error stop 8
close(u, status='delete')
print *, 'ok'
end program
