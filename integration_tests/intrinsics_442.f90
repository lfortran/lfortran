program intrinsics_442
implicit none
integer, parameter :: ascii = selected_char_kind("ascii")
character(kind=ascii, len=26) :: s
if (ascii /= 1) error stop
s = ascii_"abcdefghijklmnopqrstuvwxyz"
if (s /= "abcdefghijklmnopqrstuvwxyz") error stop
if (len(s) /= 26) error stop
print *, ascii
print *, s
end program intrinsics_442
