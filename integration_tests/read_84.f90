program read_84
! Test that formatted READ detects EOF correctly
implicit none
integer :: x, ios
integer :: unit_num

unit_num = 20
open(unit_num, file="read_84_tmp.txt", status="replace", action="write")
write(unit_num, '(I5)') 42
close(unit_num)

! Test 1: Read the value successfully
open(unit_num, file="read_84_tmp.txt", status="old", action="read")
x = 0
read(unit_num, '(I5)', iostat=ios) x
if (ios /= 0) error stop "Test 1 failed: expected successful read"
if (x /= 42) error stop "Test 1 failed: wrong value"

! Test 2: Read past EOF should set iostat < 0
x = 0
read(unit_num, '(I5)', iostat=ios) x
if (ios >= 0) error stop "Test 2 failed: expected EOF (iostat < 0)"
close(unit_num)

! Test 3: Read from empty file should set iostat < 0
open(unit_num, file="read_84_tmp.txt", status="replace", action="write")
close(unit_num)
open(unit_num, file="read_84_tmp.txt", status="old", action="read")
x = 0
read(unit_num, '(I5)', iostat=ios) x
if (ios >= 0) error stop "Test 3 failed: expected EOF from empty file"
close(unit_num, status="delete")

print *, "All read EOF tests passed"
end program
