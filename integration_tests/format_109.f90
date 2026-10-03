program format_109
! "A" editing with an integer I/O list item must transfer the full
! kind-width bytes of the internal representation, not just one byte.
use, intrinsic :: iso_fortran_env, only : int8, int16, int64
implicit none

integer :: word, i
integer(int64) :: word8
integer(int8) :: b1
integer(int8) :: alphabet(26)
integer(int16) :: h2
integer :: warr(2)
character(len=4) :: out4
character(len=8) :: out8
character(len=6) :: out6
character(len=2) :: out2
character(len=3) :: out3
character(len=26) :: out26

word = transfer("abcd", 0)
write(out4, "(a)") word
if (out4 /= "abcd") error stop 1

word8 = transfer("ABCDEFGH", 0_int64)
write(out8, "(a)") word8
if (out8 /= "ABCDEFGH") error stop 2

! aw with w > len: right-justified, blank padded
write(out6, "(a6)") word
if (out6 /= "  abcd") error stop 3

! aw with w < len: leftmost w characters
write(out2, "(a2)") word
if (out2 /= "ab") error stop 4

! bytes beyond the value must be the internal representation (zeros for 65)
word = 65
write(out4, "(a)") word
if (iachar(out4(1:1)) /= 65) error stop 5
do i = 2, 4
    if (iachar(out4(i:i)) /= 0) error stop 6
end do

! integer(int8): one byte per value, right-justified when wider
b1 = int(iachar("Z"), int8)
write(out2, "(a)") b1
if (out2 /= "Z ") error stop 7
write(out2, "(a2)") b1
if (out2 /= " Z") error stop 8

! integer(int16): two bytes
h2 = transfer("xy", 0_int16)
write(out2, "(a)") h2
if (out2 /= "xy") error stop 9
write(out3, "(a3)") h2
if (out3 /= " xy") error stop 10

! integer array: each element transfers its kind width
warr(1) = transfer("abcd", 0)
warr(2) = transfer("efgh", 0)
write(out8, "(2a)") warr
if (out8 /= "abcdefgh") error stop 11
write(out8, "(*(a))") warr
if (out8 /= "abcdefgh") error stop 12

! integer(int8) array spelling the alphabet
do i = 1, 26
    alphabet(i) = int(64 + i, int8)
end do
write(out26, "(*(a))") alphabet
if (out26 /= "ABCDEFGHIJKLMNOPQRSTUVWXYZ") error stop 13

print *, "ok"
end program format_109
