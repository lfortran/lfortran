program real_boz
implicit none

integer :: i
real :: boz_1, matrix1(5), matrix2(2), arr1(2)
integer :: arr2(2)

!Check Scalar Assigment
data boz_1 /b'01011101'/  

!Check Multiple Assigments of Real array with BOZ Values
data matrix1 / b'10', 2*o'100', 3.12, z'ABC' / 

!Check Data Broadcasting to all elements of array
data matrix2 / 2*b'10' /

!Check Implied Do loop Assignments, with real assigments along with integer assignments
data (arr1(i), arr2(i), i=1,2) / 1.2, z'02', z'01', 3 /

print *, boz_1
print *, matrix1
print *, matrix2
print *, arr1, arr2

end program
