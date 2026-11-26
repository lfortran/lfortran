program write_implied_do_4
! An unformatted WRITE whose list has an implied-DO of items of different
! types with a run-time bound: error conditions, zero-trip loops, CONVERT=
! and a UNIT= expression that is evaluated once.
implicit none
integer :: i, n, m, k(2), k2(2), last, ios, sz, marker, calls
real(8) :: z(2), z2(2)
character(len=80) :: msg
n = 2
z = [1.5d0, 2.5d0]
k = [10, 20]

! POS= is not allowed on a sequential unit: the error condition terminates
! the statement, IOSTAT= and IOMSG= report it, and nothing is written.
open(10, file='write_implied_do_4.bin', form='unformatted', status='replace')
ios = 0
msg = ''
write(10, pos=1, iostat=ios, iomsg=msg) n, (z(i), k(i), i=1,n)
if (ios == 0) error stop "pos= on a sequential unit: no error"
if (len_trim(msg) == 0) error stop "pos= on a sequential unit: no iomsg"
ios = 0
write(10, pos=1, iostat=ios) (z(i), k(i), i=1,n), n
if (ios == 0) error stop "pos= on a sequential unit: no error (loop first)"
close(10)
inquire(file='write_implied_do_4.bin', size=sz)
print *, sz
if (sz /= 0) error stop "pos= on a sequential unit: data written"

! A zero-trip loop between other items, and as the only item
m = 0
open(10, file='write_implied_do_4.bin', form='unformatted', status='replace')
write(10) 7, (z(i), k(i), i=1,m), 99
ios = 1
write(10, iostat=ios) (z(i), k(i), i=1,m)
if (ios /= 0) error stop "zero-trip loop: iostat not 0"
close(10)
inquire(file='write_implied_do_4.bin', size=sz)
print *, sz
! 4 + 8 + 4 bytes and 4 + 0 + 4 bytes
if (sz /= 24) error stop "zero-trip loop: wrong file size"
open(10, file='write_implied_do_4.bin', form='unformatted', status='old')
read(10) m, last
if (m /= 7 .or. last /= 99) error stop "zero-trip loop: wrong values"
close(10, status='delete')

! CONVERT= swaps the bytes of every part of the record and of its markers
open(10, file='write_implied_do_4.bin', form='unformatted', status='replace', &
    convert='big_endian')
write(10) n, (z(i), k(i), i=1,n)
close(10)
open(10, file='write_implied_do_4.bin', form='unformatted', status='old', &
    convert='big_endian')
read(10) m, (z2(i), k2(i), i=1,m)
close(10)
if (m /= 2 .or. any(z2 /= z) .or. any(k2 /= k)) &
    error stop "convert=: wrong values"
open(10, file='write_implied_do_4.bin', form='unformatted', status='old', &
    access='stream')
read(10) marker, m
close(10, status='delete')
print *, marker, m
! The record length 28 and the value 2, big endian
if (marker /= 469762048 .or. m /= 33554432) &
    error stop "convert=: not big endian"

! The unit is identified once per statement: a function that gives the unit
! is called once, however many iterations the loop has
calls = 0
open(10, file='write_implied_do_4.bin', form='unformatted', status='replace')
write(unit_of()) n, (z(i), k(i), i=1,n), 3
print *, calls
if (calls /= 1) error stop "unit function: not called once"
ios = 1
write(unit_of(), iostat=ios) (z(i), k(i), i=1,n), n
print *, calls
if (calls /= 2) error stop "unit function with iostat=: not called once"
if (ios /= 0) error stop "unit function with iostat=: error"
close(10)
open(10, file='write_implied_do_4.bin', form='unformatted', status='old')
z2 = 0
k2 = 0
read(10) m, (z2(i), k2(i), i=1,m), last
if (m /= 2 .or. any(z2 /= z) .or. any(k2 /= k) .or. last /= 3) &
    error stop "unit function: wrong values"
z2 = 0
k2 = 0
read(10) (z2(i), k2(i), i=1,n), m
if (m /= 2 .or. any(z2 /= z) .or. any(k2 /= k)) &
    error stop "unit function with iostat=: wrong values"
close(10, status='delete')
print *, "ok"

contains

integer function unit_of()
calls = calls + 1
unit_of = 10
end function

end program
