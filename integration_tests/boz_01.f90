program boz_01
implicit none

integer :: boz_1, boz_2, boz_3, boz_4, boz_5, boz_6

boz_1 = int(b'01011101')
boz_2 = int(o'2347')
boz_3 = int(z'ABC')
Data boz_4 /b'01011101'/
Data boz_5 /o'2347'/
Data boz_6 /z'ABC'/

print *, boz_1, boz_2, boz_3, boz_4, boz_5, boz_6
end program
