! FORALL with a defined assignment, with array components and with
! aliased variables: every right-hand side is evaluated before any
! assignment (F2018 10.2.4.2.3).
! GFortran assigns these one index value at a time, so this test is not
! run with it. Allocatable components are reallocated on assignment.
module forall_09_mod
implicit none
type :: item
    integer :: v
end type
type :: row
    integer :: arr(3)
end type
type :: buffer
    integer, allocatable :: arr(:)
end type
interface assignment(=)
    module procedure from_int
end interface
contains
pure integer function from_common(i)
integer, intent(in) :: i
integer :: c(5)
common /forall_09_blk/ c
from_common = c(6-i)
end function

pure subroutine from_int(x, n)
type(item), intent(out) :: x
integer, intent(in) :: n
x%v = n
end subroutine
end module

program forall_09
use forall_09_mod
implicit none
type(item) :: d(4)
type(row) :: s(4)
type(buffer) :: b(3)
integer :: a(5), m(3,3), k
integer, target :: t(3)
integer, pointer :: p(:)
integer :: i
integer :: e1(5), e2(5), c(5)
equivalence (e1, e2)
common /forall_09_blk/ c

do i = 1, 4
    d(i)%v = i
end do
forall (i = 1:4) d(i) = sum(d%v)
if (any(d%v /= 10)) error stop 1

do i = 1, 4
    d(i)%v = i
end do
forall (i = 1:4) d(i) = d(5-i)%v
if (any(d%v /= [4, 3, 2, 1])) error stop 2

do i = 1, 4
    s(i)%arr = [i, 2*i, 3*i]
end do
forall (i = 1:4) s(i)%arr = s(5-i)%arr
do i = 1, 4
    if (any(s(i)%arr /= [5-i, 2*(5-i), 3*(5-i)])) error stop 3
end do

do i = 1, 4
    s(i)%arr = [i, 2*i, 3*i]
end do
forall (i = 1:4) s(i)%arr = sum(s(1)%arr + s(4)%arr)
do i = 1, 4
    if (any(s(i)%arr /= 30)) error stop 4
end do

! A function that reads the variable by host association.
a = [1, 2, 3, 4, 5]
forall (i = 1:5) a(i) = a(i) + first_a()
if (any(a /= [2, 3, 4, 5, 6])) error stop 5

! Allocatable components reallocated to values of other sizes.
do i = 1, 3
    allocate(b(i)%arr(i))
    b(i)%arr = i
end do
forall (i = 1:3) b(i)%arr = b(4-i)%arr
do i = 1, 3
    if (size(b(i)%arr) /= 4-i .or. any(b(i)%arr /= 4-i)) error stop 6
end do

! Subscripts of the variable that read it through a vector subscript or
! through a pointer.
m = reshape([(k, k = 1, 9)], [3, 3])
m(3,:) = [3, 1, 2]
forall (i = 1:2) m(i, m(3,1:2)) = m(i,1:2)*10 + m(3,1:2)
if (any(m /= reshape([41, 51, 3, 4, 5, 1, 13, 23, 2], [3, 3]))) error stop 7

t = [3, 2, 1]
p => t
forall (i = 1:3) t(p(i)) = i*10
if (any(t /= [30, 20, 10])) error stop 8

! Variables aliased by an associate name, by EQUIVALENCE, or by a common
! block that a called function reads.
a = [1, 2, 3, 4, 5]
associate (x => a)
    forall (i = 1:5) a(i) = x(6-i)
end associate
if (any(a /= [5, 4, 3, 2, 1])) error stop 9

a = [1, 2, 3, 4, 5]
associate (x => a)
    forall (i = 1:5) x(i) = a(6-i)
end associate
if (any(a /= [5, 4, 3, 2, 1])) error stop 10

a = [1, 2, 3, 4, 5]
associate (x => a(3))
    forall (i = 1:5) a(i) = a(i) + x
end associate
if (any(a /= [4, 5, 6, 7, 8])) error stop 11

e1 = [1, 2, 3, 4, 5]
forall (i = 1:5) e1(i) = e2(6-i)
if (any(e1 /= [5, 4, 3, 2, 1])) error stop 12

c = [1, 2, 3, 4, 5]
forall (i = 1:5) c(i) = from_common(i)
if (any(c /= [5, 4, 3, 2, 1])) error stop 13

! A function that reads the variable through another contained function.
a = [1, 2, 3, 4, 5]
forall (i = 1:5) a(i) = a(i) + via_first_a()
if (any(a /= [2, 3, 4, 5, 6])) error stop 14
print *, "ok"

contains

pure integer function first_a()
first_a = a(1)
end function

pure integer function via_first_a()
via_first_a = first_a()
end function
end program
