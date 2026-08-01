program format_108
! "A" editing with an integer I/O list item must transfer the full
! kind-width bytes of the internal representation, not just one byte.
use, intrinsic :: iso_fortran_env, only : int64
implicit none

integer :: word, i
integer(int64) :: word8
character(len=4) :: out4
character(len=8) :: out8
character(len=6) :: out6
character(len=2) :: out2

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

print *, "ok"
end program format_108
