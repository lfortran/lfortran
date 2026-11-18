program read_list_directed_repeat_02
! List-directed READ from an external file with repeat counts r*c and
! null repeats r*
implicit none
integer :: ia(4), i, j, k, u, ios
real :: ra(3), x, y
double precision :: da(30)
logical :: la(4), l1, l2
character(len=3) :: ca(3), c1, c2
complex :: za(3), z1, z2

open(newunit=u, file='read_list_directed_repeat_02.dat', status='replace')
write(u, '(a)') '3*7'
write(u, '(a)') '4 2*5'
write(u, '(a)') '2*1.5 2.0'
write(u, '(a)') '2*2.5, 1.0'
write(u, '(a)') '30*0.d0'
write(u, '(a)') '4*F'
write(u, '(a)') '2*T'
write(u, '(a)') 'F 2*'
write(u, '(a)') "3*'a'"
write(u, '(a)') "2*ab 'x y'"
write(u, '(a)') '3*(1.0,2.0)'
write(u, '(a)') '2*(3.0, 4.0)'
write(u, '(a)') '1 2* 4'
write(u, '(a)') '1,2*,4'
write(u, '(a)') '1 2* ,4'
write(u, '(a)') '2*8 3*9'
write(u, '(a)') '5*6 99'
write(u, '(a)') '11'
write(u, '(a)') '2*5,'
write(u, '(a)') '3'
write(u, '(a)') '2*5/ 8'
write(u, '(a)') '3*4'
write(u, '(a)') '3*x'
write(u, '(a)') '5'
write(u, '(a)') '3*1.5'
write(u, '(a)') '6'
rewind(u)

ia = -9
read(u, *) ia(1:3)
if (any(ia /= [7, 7, 7, -9])) error stop 1
i = -9; j = -9; k = -9
read(u, *) i, j, k
if (i /= 4 .or. j /= 5 .or. k /= 5) error stop 2
ra = -9
read(u, *) ra
if (any(ra /= [1.5, 1.5, 2.0])) error stop 3
x = -9; y = -9; ra = -9
read(u, *) x, y, ra(1)
if (x /= 2.5 .or. y /= 2.5 .or. ra(1) /= 1.0) error stop 4
da = -9
read(u, *) da
if (any(da /= 0)) error stop 5
la = .true.
read(u, *) la
if (any(la)) error stop 6
l1 = .false.; l2 = .false.
read(u, *) l1, l2
if (.not. (l1 .and. l2)) error stop 7
la = .true.
read(u, *) la(1:3)
if (la(1) .or. .not. all(la(2:4))) error stop 25
ca = '--'
read(u, *) ca
if (any(ca /= 'a')) error stop 8
c1 = '--'; c2 = '--'; ca = '--'
read(u, *) c1, c2, ca(1)
if (c1 /= 'ab' .or. c2 /= 'ab' .or. ca(1) /= 'x y') error stop 9
za = (-9, -9)
read(u, *) za
if (any(za /= (1.0, 2.0))) error stop 10
z1 = (-9, -9); z2 = (-9, -9)
read(u, *) z1, z2
if (z1 /= (3.0, 4.0) .or. z2 /= (3.0, 4.0)) error stop 11

! r* is r null values: the items keep their values
ia = -9
read(u, *) ia
if (any(ia /= [1, -9, -9, 4])) error stop 12
i = -9; j = -9; k = -9; ia = -9
read(u, *) i, j, k, ia(1)
if (i /= 1 .or. j /= -9 .or. k /= -9 .or. ia(1) /= 4) error stop 13
! The separator after r* belongs to it, also after blanks
ra = -9; i = -9
read(u, *) ra(1:3), i
if (any(ra /= [1.0, -9.0, -9.0]) .or. i /= 4) error stop 24

! A repeat continues over the items of the statement
ia = -9; i = -9; j = -9
read(u, *) i, ia(1:3), j
if (i /= 8 .or. any(ia /= [8, 9, 9, -9]) .or. j /= 9) error stop 14
! The values of a repeat left at the end of the statement are dropped
ia = -9
read(u, *) ia(1:3)
if (any(ia /= [6, 6, 6, -9])) error stop 15
i = -9
read(u, *) i
if (i /= 11) error stop 16
! The values continue on the next record
ia = -9
read(u, *) ia(1:3)
if (any(ia /= [5, 5, 3, -9])) error stop 17
! A slash after a repeat ends the input of the statement
ia = -9
read(u, *) ia
if (any(ia /= [5, 5, -9, -9])) error stop 18
i = -9; j = -9; k = -9
read(u, *) i, j, k
if (i /= 4 .or. j /= 4 .or. k /= 4) error stop 19
! The values of a repeat left by a READ that fails are dropped too
i = -9
read(u, *, iostat=ios) i
if (ios <= 0) error stop 20
i = -9
read(u, *, iostat=ios) i
if (ios /= 0 .or. i /= 5) error stop 21
x = -9; i = -9
read(u, *, iostat=ios) x, i
if (ios <= 0 .or. x /= 1.5) error stop 22
i = -9
read(u, *, iostat=ios) i
if (ios /= 0 .or. i /= 6) error stop 23
close(u, status='delete')
print *, 'ok'
end program
