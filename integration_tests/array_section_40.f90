program array_section_40
! A nested vector subscript on the left-hand side, `a(b([1, 2])) = [5, 6]`:
! the elements of `b([1, 2])` are the subscripts of `a` and are evaluated
! once, before the assignment (F2018 10.2.1.3), which is also what makes an
! aliasing subscript such as `a(a([1, 2]))` well defined.
implicit none
integer :: a(2), b(2), c(3), d(3), m(2, 2), i

! Array right-hand side.
a = [2, 1]
b = [2, 1]
a(b([1, 2])) = [5, 6]
if (a(1) /= 6 .or. a(2) /= 5) error stop 1

! Scalar right-hand side, broadcast over the one-element subscript.
a = [2, 1]
a(b([1])) = 5
if (a(1) /= 2 .or. a(2) /= 5) error stop 2

! The subscript reads the array being assigned, so it is evaluated first.
a = [2, 1]
a(a([1, 2])) = [5, 6]
if (a(1) /= 6 .or. a(2) /= 5) error stop 3

! Inside a DO loop, with a subscript that depends on the loop variable.
c = [3, 2, 1]
d = [2, 3, 1]
do i = 1, 3
    c(d([i])) = [i * 10]
end do
if (any(c /= [30, 10, 20])) error stop 4

! One dimension of a rank-2 target.
m = reshape([1, 2, 3, 4], [2, 2])
m(1, b([1, 2])) = [7, 8]
if (m(1, 1) /= 8 .or. m(1, 2) /= 7) error stop 5
if (m(2, 1) /= 2 .or. m(2, 2) /= 4) error stop 6

print *, "ok"
end program
