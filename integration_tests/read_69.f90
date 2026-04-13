program read_69
implicit none
integer :: a, b, c, ios
character(256) :: msg

! Test 1: comma-separated integers on one line
open(10, file='read_69_test.txt', status='replace')
write(10, '(A)') '1,2'
close(10)

open(10, file='read_69_test.txt', status='old')
read(10, *, iostat=ios, iomsg=msg) a, b
close(10)

if (ios /= 0) error stop
if (a /= 1) error stop
if (b /= 2) error stop

! Test 2: comma with spaces
open(10, file='read_69_test.txt', status='replace')
write(10, '(A)') '10 , 20 , 30'
close(10)

open(10, file='read_69_test.txt', status='old')
read(10, *, iostat=ios, iomsg=msg) a, b, c
close(10)

if (ios /= 0) error stop
if (a /= 10) error stop
if (b /= 20) error stop
if (c /= 30) error stop

! Test 3: comma-separated on multiple reads
open(10, file='read_69_test.txt', status='replace')
write(10, '(A)') '100,200'
write(10, '(A)') '300'
close(10)

open(10, file='read_69_test.txt', status='old')
read(10, *) a, b
read(10, *) c
close(10)

if (a /= 100) error stop
if (b /= 200) error stop
if (c /= 300) error stop

! Cleanup
open(10, file='read_69_test.txt', status='old')
close(10, status='delete')

print *, "PASS"
end program
