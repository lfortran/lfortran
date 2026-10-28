! `t = matmul(x, v) + b` must not compute the matmul into `t` first when
! `t` shares its storage with an operand through COMMON or EQUIVALENCE.
module matmul_16_mod
implicit none
real :: t5(2), b5(2)
common /c5/ t5, b5
end module

program matmul_16
use matmul_16_mod
implicit none
type sq
    sequence
    real :: c(2)
end type
real :: t1(2), b1(2)
real :: t2(2)
integer :: t3(2)
real :: t4(2), b4(2)
real :: q5(4)
real :: t6(2), b6(2)
type(sq) :: s7
real :: x(2,2), v(2)
common /c1/ t1, b1
common /c2/ t2
common /c3/ t3
common /c4/ t4
common /c5/ q5
common /c6/ t6
equivalence (t6, b6)
common /c7/ s7

x = reshape([1., 2., 3., 4.], [2, 2])
v = [1., 1.]

! A view of the internal procedure with another layout: q(1:2) is t1.
t1 = [10., 20.]; b1 = [30., 40.]
call s1()
print *, t1
if (any(t1 /= [14., 26.])) error stop 1

! The whole-array view q of the internal procedure is t2.
t2 = [10., 20.]
call s2()
print *, t2
if (any(t2 /= [14., 26.])) error stop 2

! The subscript k of the internal procedure is t3(1).
t3 = [1, 0]
call s3()
print *, t3
if (any(t3 /= [104, 206])) error stop 3

! The matmul argument q of the internal procedure is t4.
t4 = [1., 2.]; b4 = 0
call s4()
print *, t4
if (any(t4 /= [7., 10.])) error stop 4

! A module COMMON viewed with another layout: q5(1:2) is t5.
t5 = [10., 20.]; b5 = [30., 40.]
t5 = matmul(x, v) + q5(1:2)
print *, t5
if (any(t5 /= [14., 26.])) error stop 5

! EQUIVALENCE of a COMMON member: b6 is t6.
t6 = [10., 20.]
t6 = matmul(x, v) + b6
print *, t6
if (any(t6 /= [14., 26.])) error stop 6

! A SEQUENCE-type COMMON member: q of the internal procedure is s7%c.
s7%c = [10., 20.]
call s7_sub()
print *, s7%c
if (any(s7%c /= [14., 26.])) error stop 7

contains

subroutine s1()
real :: q(4)
common /c1/ q
t1 = matmul(x, v) + q(1:2)
end subroutine

subroutine s2()
real :: q(2)
common /c2/ q
t2 = matmul(x, v) + q
end subroutine

subroutine s3()
integer :: k, kk, b(5), xi(2,2), vi(2)
common /c3/ k, kk
xi = reshape([1, 2, 3, 4], [2, 2])
vi = [1, 1]
b = [100, 200, 300, 400, 500]
t3 = matmul(xi, vi) + b(k:k+1)
end subroutine

subroutine s4()
real :: q(2)
common /c4/ q
t4 = matmul(x, q) + b4
end subroutine

subroutine s7_sub()
real :: q(2)
common /c7/ q
s7%c = matmul(x, v) + q
end subroutine

end program
