! The elements of non-contiguous input items that a READ statement transfers
! before an end-of-file condition are kept, as are the items it does not reach
program read_107
implicit none
integer, target :: a(10)
integer, pointer :: p(:)
integer :: ios, u
character(len=20) :: s

p => a(1:10:2)
s = '1 2 3'
a = -1
read(s, *, iostat=ios) p
if (ios >= 0) error stop 1
if (any(a /= [1, -1, 2, -1, 3, -1, -1, -1, -1, -1])) error stop 2
a = -1
read(s, *, end=10) p
error stop 3
10 continue
if (any(a /= [1, -1, 2, -1, 3, -1, -1, -1, -1, -1])) error stop 4

s = '1 2 3 4'
a = -1
read(s, *, iostat=ios) p(1:3), a(2), a(4)
if (ios == 0) error stop 5
if (any(a /= [1, 4, 2, -1, 3, -1, -1, -1, -1, -1])) error stop 6

open(newunit=u, status='scratch', form='unformatted', access='stream')
write(u) 7, 8
rewind(u)
a = -1
read(u, end=20) p
error stop 7
20 continue
if (any(a /= [7, -1, 8, -1, -1, -1, -1, -1, -1, -1])) error stop 8
rewind(u)
a = -1
read(u, err=30, end=30, iostat=ios) p
error stop 9
30 continue
if (ios >= 0) error stop 10
if (any(a /= [7, -1, 8, -1, -1, -1, -1, -1, -1, -1])) error stop 11
close(u)
print *, 'ok'
end program
