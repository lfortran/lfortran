program boz_01
implicit none

integer :: boz_1, boz_2, boz_3, boz_4, boz_5

boz_1 = int(b'01011101')
boz_2 = int(o'2347')
boz_3 = int(z'ABC')
!Testing Truncation of BOZ
boz_4 = int(Z'234567890abcdef1')
boz_5 = int(Z'2234567890abcdef1')
if (boz_4 /= boz_5) stop

print *, boz_1, boz_2, boz_3
end program
