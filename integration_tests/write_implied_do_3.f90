program write_implied_do_3
! An unformatted WRITE whose list has an implied-DO of items of different
! types with a run-time bound writes one record with all items in order.
implicit none
integer :: i, j, m, n, k(2), k2(2), last, marker(6)
real(8) :: z(2), z2(2)
real :: a(2, 2), a2(2, 2)
m = 2
z = [1.5d0, 2.5d0]
k = [10, 20]
a = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])
open(10, file='write_implied_do_3.bin', form='unformatted', status='replace')
write(10) m, (z(i), k(i), i=1,m), 99
write(10) (z(i), k(i), i=1,m)
write(10) (k(i), (a(j,i), j=1,m), z(i), i=1,m)
close(10)

! Each WRITE is one record: 4 + 2*12 + 4 = 32, 2*12 = 24, 2*(4 + 8 + 8) = 40
open(10, file='write_implied_do_3.bin', access='stream', form='unformatted', &
    status='old')
read(10, pos=1) marker(1)
read(10, pos=37) marker(2)
read(10, pos=41) marker(3)
read(10, pos=69) marker(4)
read(10, pos=73) marker(5)
read(10, pos=117) marker(6)
close(10)
print *, marker
if (any(marker /= [32, 32, 24, 24, 40, 40])) error stop "wrong record length"

open(10, file='write_implied_do_3.bin', form='unformatted', status='old')
read(10) n, (z2(i), k2(i), i=1,n), last
if (n /= 2 .or. any(z2 /= z) .or. any(k2 /= k) .or. last /= 99) &
    error stop "wrong values in record 1"
z2 = 0; k2 = 0
read(10) (z2(i), k2(i), i=1,m)
if (any(z2 /= z) .or. any(k2 /= k)) error stop "wrong values in record 2"
z2 = 0; k2 = 0; a2 = 0
read(10) (k2(i), (a2(j,i), j=1,m), z2(i), i=1,m)
if (any(z2 /= z) .or. any(k2 /= k) .or. any(a2 /= a)) &
    error stop "wrong values in record 3"
close(10, status='delete')

! Stream access with POS=: the items follow each other from that position
open(11, file='write_implied_do_3.str', access='stream', form='unformatted', &
    status='replace')
write(11, pos=5) (z(i), k(i), i=1,m)
z2 = 0; k2 = 0
read(11, pos=5) z2(1), k2(1), z2(2), k2(2)
close(11, status='delete')
if (any(z2 /= z) .or. any(k2 /= k)) error stop "wrong values with pos="
print *, "ok"
end program
