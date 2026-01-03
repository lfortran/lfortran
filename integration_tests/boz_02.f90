!This file contains tests for BOZ, which are not yet supported in gfortran
program real_boz
implicit none

!check DATA Assignments
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


!Check Declaration Statements
!Check Scalar Initializations
real::a = o'10'
integer::b = z'10'
!Check Parameter Initializations
real,parameter::c = b'01'
integer,parameter::d = z'02'
!Check Array Initializations
real:: e(3) = [real::b'01', z'10', 3.4]
integer:: f(3) = [integer::1, b'10', 4]

!Check for BOZ Assignments
integer :: boz_6
integer :: boz_arr_int(2)
real:: boz_7
real:: boz_arr_real(2)

!Check Integer Array Assignments
boz_arr_int(1) = b'11'
boz_arr_int(2) = 1

!Check Real Array Assignments
boz_arr_real(1) = z'01'
boz_arr_real(2) = 1.2

!Check Integer Scalar Assignments
boz_6 = o'02'
!Check Real Scalar Assignments
boz_7 = z'03'

print *, boz_1
print *, matrix1
print *, matrix2
print *, arr1, arr2
print *, a,b,c,d,e,f
print *, boz_6, boz_7
print *, boz_arr_int, boz_arr_real

end program
