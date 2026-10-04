program derived_types_194
implicit none

type :: inner
    integer :: ii = 9
    integer :: v(2) = [3, 4]
end type inner

type :: outer
    integer :: u(2) = [5, 6]
    type(inner) :: nest
    integer :: tail = 77
end type outer

type(outer) :: w(2)
type(outer) :: m(2,3)
type(outer) :: q(2:4)
integer :: i, j
integer :: r(2)

! indexing before the component keeps working
w(1)%u(2) = 1
if (w(1)%u(2) /= 1) error stop "index first: wrong assignment"
if (w(2)%u(2) /= 6) error stop "index first: other element clobbered"
w(1)%u(2) = 6

! an element of an array component of a whole array
w%u(2) = 7
if (w(1)%u(2) /= 7) error stop "array component: element 1 not assigned"
if (w(2)%u(2) /= 7) error stop "array component: element 2 not assigned"
if (w(1)%u(1) /= 5) error stop "array component: neighbour 1 clobbered"
if (w(2)%u(1) /= 5) error stop "array component: neighbour 2 clobbered"

! and reading it back
if (any(w%u(2) /= 7)) error stop "array component: wrong values read"
if (sum(w%u(2)) /= 14) error stop "array component: wrong sum"

! an element of an array component reached through a nested member
w%nest%v(2) = 8
if (w(1)%nest%v(2) /= 8) error stop "nested array component: element 1 not assigned"
if (w(2)%nest%v(2) /= 8) error stop "nested array component: element 2 not assigned"
if (w(1)%nest%v(1) /= 3) error stop "nested array component: neighbour 1 clobbered"
if (w(2)%nest%v(1) /= 3) error stop "nested array component: neighbour 2 clobbered"
if (any(w%nest%ii /= 9)) error stop "nested array component: sibling clobbered"
if (any(w%tail /= 77)) error stop "nested array component: next component clobbered"

! and reading it back
if (any(w%nest%v(2) /= 8)) error stop "nested array component: wrong values read"
if (sum(w%nest%v(2)) /= 16) error stop "nested array component: wrong sum"

! an array right-hand side is spread element by element
w%nest%v(1) = [21, 22]
if (w(1)%nest%v(1) /= 21) error stop "array rhs: element 1 wrong"
if (w(2)%nest%v(1) /= 22) error stop "array rhs: element 2 wrong"
if (any(w%nest%v(2) /= 8)) error stop "array rhs: neighbour clobbered"

! the subscript may be a variable
i = 2
w%nest%v(i) = 12
if (w(1)%nest%v(2) /= 12) error stop "variable subscript: element 1 not assigned"
if (w(2)%nest%v(2) /= 12) error stop "variable subscript: element 2 not assigned"
if (w(1)%nest%v(1) /= 21) error stop "variable subscript: neighbour 1 clobbered"
if (w(2)%nest%v(1) /= 22) error stop "variable subscript: neighbour 2 clobbered"

! a rank-2 array
do j = 1, 3
    do i = 1, 2
        m(i,j)%nest%v(2) = 0
    end do
end do
m%nest%v(2) = 4
if (any(m%nest%v(2) /= 4)) error stop "rank 2: wrong assignment"
if (sum(m%nest%v(2)) /= 24) error stop "rank 2: wrong sum"
if (any(m%nest%v(1) /= 3)) error stop "rank 2: neighbour clobbered"

! used in an array expression and in an intrinsic
r = w%u(2) + w%nest%v(1)
if (r(1) /= 28) error stop "array expression: element 1 wrong"
if (r(2) /= 29) error stop "array expression: element 2 wrong"
if (maxval(w%nest%v(1)) /= 22) error stop "intrinsic: wrong maxval"
if (minval(w%nest%v(1)) /= 21) error stop "intrinsic: wrong minval"

! the reference has as many elements as the base it is taken from
if (size(w%nest%v(1)) /= 2) error stop "inquiry: wrong size"
if (size(m%nest%v(2)) /= 6) error stop "inquiry: wrong size of rank 2"

! a base whose bounds do not start at one: the reference has as many
! elements as the base, but its own bounds start at one
do i = 2, 4
    q(i)%u = [i, i*2]
end do
q%u(2) = 9
if (q(2)%u(2) /= 9) error stop "shifted base: element 1 not assigned"
if (q(3)%u(2) /= 9) error stop "shifted base: element 2 not assigned"
if (q(4)%u(2) /= 9) error stop "shifted base: element 3 not assigned"
if (q(2)%u(1) /= 2) error stop "shifted base: neighbour 1 clobbered"
if (q(4)%u(1) /= 4) error stop "shifted base: neighbour 3 clobbered"
if (lbound(q%u(2), 1) /= 1) error stop "shifted base: wrong lower bound"
if (ubound(q%u(2), 1) /= 3) error stop "shifted base: wrong upper bound"
if (size(q%u(2)) /= 3) error stop "shifted base: wrong size"

print *, w%u(2)
print *, w%nest%v(2)
print *, m%nest%v(2)
print *, q%u(2)

end program derived_types_194
