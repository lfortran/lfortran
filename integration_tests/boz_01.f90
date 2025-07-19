program boz_01
implicit none

integer :: boz_1, boz_2, boz_3
integer::i
real :: matrix1(6)
real :: arr1(2)
real::boz_4
integer:: arr2(2)
Data matrix1/ 3.2, b'10', 2*o'100' /
Data (arr1(i), arr2(i), i=1,2) / 1.2, z'02', z'01', 3 /
Data boz_4 /b'01011101'/

boz_1 = int(b'01011101')
boz_2 = int(o'2347')
boz_3 = int(z'ABC')

print *, boz_1, boz_2, boz_3
print *, matrix1
print *, arr1,arr2
print *, boz_4
end program
