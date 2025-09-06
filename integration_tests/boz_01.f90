program boz_01
implicit none

integer :: boz_1, boz_2, boz_3, boz_4, boz_5
real:: boz_6

boz_1 = int(b'01011101')
boz_2 = int(o'2347')
boz_3 = int(z'ABC')
!Testing Truncation of BOZ
boz_4 = int(Z'234567890abcdef1')
boz_5 = int(Z'2234567890abcdef1')
!Checking Real Conversion of BOZ
boz_6 = real(b'01101')
!Check with Integer Equivalent Values
if (boz_4 /= boz_5) error stop
if (boz_1 /= 93) error stop
if (boz_2 /= 1255) error stop
if (boz_3 /= 2748) error stop
if (boz_4 /= 180150001) error stop
if (boz_5 /= 180150001) error stop 

print *, boz_1, boz_2, boz_3, boz_4, boz_5, boz_6
end program
