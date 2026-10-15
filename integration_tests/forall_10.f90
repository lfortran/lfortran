! FORALL with vector subscripts of the assigned variable that read it:
! the subscripts are evaluated for every index value before any
! assignment (F2018 10.2.4.2.3).
! GFortran evaluates these subscripts one index value at a time, so this
! test is not run with it.
program forall_10
implicit none
type :: t
    integer :: v(3)
end type
integer :: a(3), b(2,3), c(4), e(6), i
type(t) :: s

a = [2, 3, 1]
forall (i = 1:3) a(a(i:i)) = [i]
if (any(a /= [3, 1, 2])) error stop 1

a = [2, 3, 1]
forall (i = 1:3) a(a([i])) = [i]
if (any(a /= [3, 1, 2])) error stop 2

a = [2, 3, 1]
forall (i = 1:3) a(a(i:i) + 0) = [i]
if (any(a /= [3, 1, 2])) error stop 3

a = [2, 3, 1]
forall (i = 1:3) a(a(i:i)) = i
if (any(a /= [3, 1, 2])) error stop 4

a = [2, 3, 1]
forall (i = 1:3) a(a(i:i)) = a(i:i)*10 + i
if (any(a /= [13, 21, 32])) error stop 5

a = [2, 3, 1]
forall (i = 1:3, a(i) > 1) a(a(i:i)) = [i*10]
if (any(a /= [2, 10, 20])) error stop 6

b(1,:) = [2, 3, 1]
b(2,:) = [3, 1, 2]
forall (i = 1:2) b(i, b(3-i,:)) = [3, 2, 1]
if (any(b(1,:) /= [2, 1, 3])) error stop 7
if (any(b(2,:) /= [1, 3, 2])) error stop 8

c = [3, 4, 1, 2]
forall (i = 1:2) c(c(2*i-1:2*i)) = [i, -i]
if (any(c /= [2, -2, 1, -1])) error stop 9

! The vector subscripts have different sizes.
c = [3, 4, 1, 2]
forall (i = 1:2) c(c(i:2*i-1)) = 10*i
if (any(c /= [20, 4, 10, 20])) error stop 10

! The size of the vector subscript of i = 2 reads e(2), which i = 1 assigns.
e = [3, 5, 2, 1, 1, 6]
forall (i = 1:2) e(e(2*i+1:e(i))) = 10*i
if (any(e /= [20, 10, 2, 1, 1, 6])) error stop 11

s%v = [2, 3, 1]
forall (i = 1:3) s%v(s%v(i:i)) = [i]
if (any(s%v /= [3, 1, 2])) error stop 12

print *, "ok"
end program
