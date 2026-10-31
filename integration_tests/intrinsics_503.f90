! Runtime ichar() for the C/C++ backends: code of the (single)
! character, zero-extended to the result kind.
program intrinsics_503
implicit none
character(len=1) :: c
character(len=1) :: d
c = "A"
d = "z"
if (ichar(c) /= 65) error stop 1
if (ichar(d) /= 122) error stop 2
if (ichar("A") /= 65) error stop 3
if (ichar(c, 8) /= 65_8) error stop 4
print *, ichar(c), ichar(d)
end program intrinsics_503
