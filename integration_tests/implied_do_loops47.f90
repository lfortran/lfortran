program implied_do_loops47
implicit none
integer :: i
character(len=2) :: fixed(3)
character(len=:), allocatable :: a, b, c
character(len=64) :: line

! Every value of an implied do loop must be written at its own length,
! not padded (or truncated) to the length of one of the other values.
write(line, '(*(g0))') ('XXXX', 'YYY', 'ZZ', i=1,2)
if (line /= "XXXXYYYZZXXXXYYYZZ") error stop "literals, longest first"

write(line, '(*(g0))') ('ZZ', 'YYY', 'XXXX', i=1,2)
if (line /= "ZZYYYXXXXZZYYYXXXX") error stop "literals, shortest first"

a = 'XXXX'
b = 'YYY'
c = 'ZZ'
write(line, '(*(g0))') (a, b, c, i=1,2)
if (line /= "XXXXYYYZZXXXXYYYZZ") error stop "deferred length, longest first"

write(line, '(*(g0))') (c, b, a, i=1,2)
if (line /= "ZZYYYXXXXZZYYYXXXX") error stop "deferred length, shortest first"

! A value list of one and the same length must keep on working
fixed(1) = 'ab'
fixed(2) = 'cd'
fixed(3) = 'ef'
write(line, '(*(g0))') (fixed(i), fixed(i), i=1,3)
if (line /= "ababcdcdefef") error stop "equal lengths"

! Values of other types in the list must not disturb the character values
write(line, '(*(g0))') ('XXXX', 'YYY', 'ZZ', 7, i=1,2)
if (line /= "XXXXYYYZZ7XXXXYYYZZ7") error stop "mixed with an integer"

! An explicit edit descriptor per value keeps its own width
write(line, '(a4,a3,a2)') ('XXXX', 'YYY', 'ZZ', i=1,1)
if (line /= "XXXXYYYZZ") error stop "explicit format"

! Values outside the implied do loop are written along with it
write(line, '(*(g0))') 'pre', ('XXXX', 'YYY', 'ZZ', i=1,2), 'post'
if (line /= "preXXXXYYYZZXXXXYYYZZpost") error stop "sibling values"

! List directed output writes the values at their own length as well
write(line, *) ('XXXX', 'YYY', 'ZZ', i=1,2)
if (adjustl(line) /= "XXXXYYYZZXXXXYYYZZ") error stop "list directed"

print *, "ok"
end program implied_do_loops47
