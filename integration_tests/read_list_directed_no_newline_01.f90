program read_list_directed_no_newline_01
implicit none
integer :: u, n, ios
open(newunit=u, file='read_list_directed_no_newline_01.txt', &
    form='unformatted', access='stream', status='replace')
write(u) '42'
close(u)

open(newunit=u, file='read_list_directed_no_newline_01.txt', status='old')
n = 0
read(u, *, iostat=ios) n
print *, ios, n
if (ios /= 0 .or. n /= 42) error stop

read(u, *, iostat=ios)
print *, ios
if (ios /= -1) error stop

rewind(u)
read(u, *, iostat=ios)
print *, ios
if (ios /= 0) error stop
close(u, status='delete')
end program
