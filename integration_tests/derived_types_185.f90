program derived_types_185
implicit none

type :: deep
    integer :: dd = 7
end type deep

type :: inner
    integer :: ii = 9
    integer :: v(2) = [3, 4]
    type(deep) :: d
end type inner

type :: outer
    integer :: id = 0
    type(inner) :: nest
end type outer

type(outer) :: w(2)
type(outer) :: m(2,3)
integer :: i, j

! a component of the array itself keeps working
w%id = 5
if (any(w%id /= 5)) error stop "single level: wrong assignment"

! indexing before the nested member keeps working
w(1)%nest%ii = 11
if (w(1)%nest%ii /= 11) error stop "index first: wrong assignment"
if (w(2)%nest%ii /= 9) error stop "index first: other element clobbered"

! whole-array assignment to a nested member
w%nest%ii = 1
if (w(1)%nest%ii /= 1) error stop "nested member: element 1 not assigned"
if (w(2)%nest%ii /= 1) error stop "nested member: element 2 not assigned"
if (any(w%id /= 5)) error stop "nested member: neighbouring component clobbered"

! whole-array read of a nested member
if (any(w%nest%ii /= 1)) error stop "nested member: wrong values read"
if (sum(w%nest%ii) /= 2) error stop "nested member: wrong sum"

! an array right-hand side is spread element by element
w%nest%ii = [21, 22]
if (w(1)%nest%ii /= 21) error stop "array rhs: element 1 wrong"
if (w(2)%nest%ii /= 22) error stop "array rhs: element 2 wrong"

! three levels of nesting
w%nest%d%dd = 6
if (w(1)%nest%d%dd /= 6) error stop "three levels: element 1 not assigned"
if (w(2)%nest%d%dd /= 6) error stop "three levels: element 2 not assigned"
if (any(w%nest%d%dd /= 6)) error stop "three levels: wrong values read"
w%nest%d%dd = [31, 32]
if (sum(w%nest%d%dd) /= 63) error stop "three levels: wrong sum"

! the nested member is itself an array, indexed after the array element
do i = 1, 2
    if (w(i)%nest%v(1) /= 3) error stop "nested array member: wrong default"
    if (w(i)%nest%v(2) /= 4) error stop "nested array member: wrong default"
    w(i)%nest%v(2) = 8
end do
do i = 1, 2
    if (w(i)%nest%v(2) /= 8) error stop "nested array member: wrong assignment"
    if (w(i)%nest%v(1) /= 3) error stop "nested array member: neighbour clobbered"
end do

! a rank-2 array
do j = 1, 3
    do i = 1, 2
        m(i,j)%nest%ii = 0
    end do
end do
m%nest%ii = 2
if (any(m%nest%ii /= 2)) error stop "rank 2: wrong assignment"
if (sum(m%nest%ii) /= 12) error stop "rank 2: wrong sum"

print *, w%nest%ii
print *, w%nest%d%dd
print *, m%nest%ii
end program derived_types_185
