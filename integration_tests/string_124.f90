! String concatenation (//) and adjustl/adjustr via the StringConcat,
! Adjustl and Adjustr elemental intrinsics, exercised with runtime values
! so they are not constant-folded.
program string_124
implicit none
character(len=10) :: a, b
character(len=20) :: c
a = "hello"
b = "world"
c = a // b
print *, c
if (c /= "hello     world     ") error stop 1
if (a // "!" /= "hello     !") error stop 2
a = "  hi"
if (adjustl(a) /= "hi        ") error stop 3
if (adjustr(a) /= "        hi") error stop 4
print *, adjustl(a), adjustr(a)
end program string_124
