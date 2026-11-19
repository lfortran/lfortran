program read_list_directed_repeat_01
! List-directed internal READ with repeat counts r*c and null repeats r*
implicit none
integer :: ia(4), i, j, k, ios
real :: ra(3), x, y
logical :: la(4)
character(len=3) :: ca(3)
complex :: za(3), z1, z2
character(len=40) :: line
character(len=4) :: recs(2)

line = '3*7'
ia = -9
read(line, *, iostat=ios) ia(1:3)
if (ios /= 0) error stop 1
if (any(ia /= [7, 7, 7, -9])) error stop 2
i = -9; j = -9; k = -9
read(line, *) i, j, k
if (i /= 7 .or. j /= 7 .or. k /= 7) error stop 3
ia = -9; i = -9
read(line, *) ia(1:2), i
if (any(ia /= [7, 7, -9, -9]) .or. i /= 7) error stop 4

line = '2*1.5 2.0'
ra = -9
read(line, *) ra
if (any(ra /= [1.5, 1.5, 2.0])) error stop 5
x = -9; y = -9
read(line, *) x, y
if (x /= 1.5 .or. y /= 1.5) error stop 6

line = '4*F'
la = .true.
read(line, *) la
if (any(la)) error stop 7

line = "3*'a'"
ca = '--'
read(line, *) ca
if (any(ca /= 'a')) error stop 8
line = '2*abc xy'
ca = '--'
read(line, *) ca
if (ca(1) /= 'abc' .or. ca(2) /= 'abc' .or. ca(3) /= 'xy') error stop 9

line = '3*(1.0,2.0)'
za = (-9, -9)
read(line, *) za
if (any(za /= (1.0, 2.0))) error stop 10
z1 = (-9, -9); z2 = (-9, -9)
read(line, *) z1, z2
if (z1 /= (1.0, 2.0) .or. z2 /= (1.0, 2.0)) error stop 11

! r* is r null values: the items keep their values
line = '1 2* 4'
ia = -9
read(line, *, iostat=ios) ia
if (ios /= 0) error stop 12
if (any(ia /= [1, -9, -9, 4])) error stop 13
line = '1,2*,4'
ia = -9
read(line, *) ia
if (any(ia /= [1, -9, -9, 4])) error stop 14
line = '1 1* 4'
i = -9; j = -9; k = -9
read(line, *) i, j, k
if (i /= 1 .or. j /= -9 .or. k /= 4) error stop 15

! Values of a repeat left at the end of the statement are dropped
line = '5*7 8'
ia = -9
read(line, *) ia(1:3)
if (any(ia /= [7, 7, 7, -9])) error stop 16
line = '3*7 8'
i = -9
read(line, *) i
ia = -9
read(line, *) ia
if (i /= 7 .or. any(ia /= [7, 7, 7, 8])) error stop 17

! A slash after a repeat ends the input
line = '2*5/'
ia = -9
read(line, *, iostat=ios) ia
if (ios /= 0) error stop 18
if (any(ia /= [5, 5, -9, -9])) error stop 19
i = -9; j = -9; k = -9
read(line, *) i, j, k
if (i /= 5 .or. j /= 5 .or. k /= -9) error stop 20

! A repeat in an internal file with several records
recs(1) = '2*5'
recs(2) = '3'
i = -9; j = -9; k = -9
read(recs, *) i, j, k
if (i /= 5 .or. j /= 5 .or. k /= 3) error stop 21

! A '*' inside a character value is not a repeat
line = "'a*b' x*y"
ca = '--'
read(line, *) ca(1:2)
if (ca(1) /= 'a*b' .or. ca(2) /= 'x*y') error stop 22

print *, 'ok'
end program
