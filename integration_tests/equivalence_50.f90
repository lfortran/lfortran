! Operands read in place must not share storage with the assignment target
! through EQUIVALENCE (#16215)
module equivalence_50_m
implicit none
integer :: z(3), i
equivalence (z(1), i)
end module

program equivalence_50
use equivalence_50_m, only: z, i
implicit none
type t
    integer :: x(3)
end type
type(t) :: arr(3)
real :: a(5), c(4), d(4), e(4)
integer :: k, y(3)
equivalence (a(2), c(1))
equivalence (d, e)
equivalence (k, y)

arr(1)%x = [1, 2, 3]
arr(2)%x = [10, 20, 30]
arr(3)%x = [100, 200, 300]

! The subscript i is z(1), which the assignment writes first
z = 0
i = 1
z = arr(i)%x + 1
print *, z
if (any(z /= [2, 3, 4])) error stop 1
z = 0
i = 2
z = arr(i)%x * 2
print *, z
if (any(z /= [20, 40, 60])) error stop 2

! The subscript k is y(1)
y = 0
k = 1
y = arr(k)%x * 2
print *, y
if (any(y /= [2, 4, 6])) error stop 3

! Plain variables: d and e are the same storage, c(j) is a(j + 1)
d = [1., 10., 100., 1000.]
e = d + 1
print *, d
if (any(d /= [2., 11., 101., 1001.])) error stop 4
a = [1., 10., 100., 1000., 1e4]
a(1:3) = c(1:3) * 10
print *, a
if (any(a /= [100., 1000., 1e4, 1000., 1e4])) error stop 5
a = [1., 10., 100., 1000., 1e4]
a(1:4) = c + a(1:4)
print *, a
if (any(a /= [11., 110., 1100., 11000., 1e4])) error stop 6
a = [1., 10., 100., 1000., 1e4]
c = a(2:5) * 2
print *, a
if (any(a /= [1., 20., 200., 2000., 2e4])) error stop 7
end program
