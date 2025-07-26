program boz_01
implicit none

integer :: boz_1, boz_2, boz_3 
real:: boz_4
real :: matrix1(5)
real:: matrix2(5)
real :: arr1(2)
integer:: arr2(2)
integer::i

boz_1 = int(b'01011101')
boz_2 = int(o'2347')
boz_3 = int(z'ABC')
data boz_4 /b'01011101'/
data matrix1/ b'10', 2*o'100', 3.12, z'ABC' /
data matrix2/ 5*b'10'/
data (arr1(i), arr2(i), i=1,2) / 1.2, z'02', z'01', 3 /

print *, boz_1, boz_2, boz_3, boz_4
print *, matrix1
print *, matrix2
print *, arr1,arr2
end program
