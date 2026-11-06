! An internal procedure indexes an assumed-size dummy of its host with the
! bounds the host declares: a lower bound other than 1, leading extents and
! bounds that are other dummies of the host (#16138), and associates
! pointers with its sections (#15791), also for a bind(c) host
module host_assoc_13_m
use iso_c_binding, only: c_int
implicit none
contains
subroutine s1(x, r)
real :: x(2:*), r
call inner()
contains
subroutine inner()
r = x(3)
x(4) = -x(4)
end subroutine
end subroutine
subroutine s2(y, n, r)
integer :: n
real :: y(n, *), r
call inner()
contains
subroutine inner()
r = y(2, 2)
y(1, 3) = 50
end subroutine
end subroutine
subroutine s3(z, lo, n)
integer :: lo, n
real :: z(0:n, lo:*)
call inner()
contains
subroutine inner()
z(1, lo + 1) = 99
end subroutine
end subroutine
subroutine s5(x, lo)
integer :: lo
real :: x(lo:*)
call mid()
contains
subroutine mid()
call deep()
end subroutine
subroutine deep()
x(lo + 2) = 77
end subroutine
end subroutine
subroutine s6(z, n, a)
integer, intent(in) :: n
integer, target :: z(n, *)
real, target :: a(*)
call inner()
contains
subroutine inner()
integer, pointer :: q(:), q2(:, :)
real, pointer :: p(:), p2(:, :)
p => a(1:4)
if (sum(p) /= 10 .or. size(p) /= 4) error stop 8
q => z(:, 2)
if (q(1) /= 5 .or. size(q) /= 4) error stop 9
q2 => z(:, 2:3)
if (q2(1, 2) /= 9 .or. size(q2) /= 8) error stop 10
p2(1:2, 1:2) => a(1:4)
if (p2(2, 2) /= 4) error stop 11
q(1:8) => z(:, 2:3)
if (q(1) /= 5 .or. q(8) /= 12) error stop 12
end subroutine
end subroutine
subroutine s7(p, m, s) bind(c)
integer(c_int) :: p(*), m, s
call inner()
contains
subroutine inner()
s = p(1) + p(m)
end subroutine
end subroutine
subroutine s8(q, n, s) bind(c)
integer(c_int) :: n, s
integer(c_int) :: q(0:n, *)
call inner()
contains
subroutine inner()
s = q(1, 3)
q(0, 3) = 60
end subroutine
end subroutine
end module

program host_assoc_13
use host_assoc_13_m
implicit none
real :: a(4), b(2, 3), r1, r2, z(2, 3)
integer, target :: c(4, 3)
integer :: i
real, target :: x(6)
a = [1, 2, 3, 4]
b = reshape([1, 2, 3, 4, 5, 6], [2, 3])
call s1(a, r1)
call s2(b, 2, r2)
if (r1 /= 2) error stop 1
if (r2 /= 4) error stop 2
if (any(a /= [1., 2., -3., 4.])) error stop 3
if (b(1, 3) /= 50) error stop 4
c = reshape([(i, i = 1, 12)], [4, 3])
z = 0
call s3(z, 5, 1)
if (z(2, 2) /= 99 .or. count(z /= 0) /= 1) error stop 5
a = 0
call s5(a, 3)
if (a(3) /= 77 .or. count(a /= 0) /= 1) error stop 7
x = [1, 2, 3, 4, 5, 6]
call s6(c, 4, x)
call s7(c, 3, i)
if (i /= 4) error stop 13
call s8(c, 1, i)
if (i /= 6 .or. c(1, 2) /= 60) error stop 14
print *, r1, r2
end program
