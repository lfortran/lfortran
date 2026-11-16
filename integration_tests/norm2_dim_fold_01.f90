program norm2_dim_fold_01
! norm2 with dim of a constant array is folded along dim (#17872)
implicit none
real(4), parameter :: a(2, 2) = reshape([3.0, 4.0, 6.0, 8.0], [2, 2])
real(8), parameter :: b(2, 2) = reshape([3.0d0, 4.0d0, 6.0d0, 8.0d0], [2, 2])
real(4), parameter :: c(2, 2, 2) = reshape([3.0, 4.0, 6.0, 8.0, &
    5.0, 12.0, 8.0, 15.0], [2, 2, 2])
real(4), parameter :: a1(2) = norm2(a, dim=1), a2(2) = norm2(a, dim=2)
real(8), parameter :: b1(2) = norm2(b, dim=1), b2(2) = norm2(b, dim=2)
real(4), parameter :: c1(2, 2) = norm2(c, dim=1)
real(4), parameter :: c3(2, 2) = norm2(c, dim=3)
real(16), parameter :: q(2, 2) = reshape([3.0_16, 4.0_16, 6.0_16, 8.0_16], [2, 2])
real(16), parameter :: q1(2) = norm2(q, dim=1)
real(4) :: x(2), y(2, 2), cv(2, 2, 2)
real(8) :: z(2)

if (any(abs(a1 - [5.0, 10.0]) > 1e-5)) error stop 1
if (any(abs(a2 - [sqrt(45.0), sqrt(80.0)]) > 1e-5)) error stop 2
if (any(abs(b1 - [5.0d0, 10.0d0]) > 1d-12)) error stop 3
if (any(abs(b2 - [sqrt(45.0d0), sqrt(80.0d0)]) > 1d-12)) error stop 4
if (any(abs(c1 - reshape([5.0, 10.0, 13.0, 17.0], [2, 2])) > 1e-5)) error stop 5
if (any(abs(c3 - reshape([sqrt(34.0), sqrt(160.0), sqrt(100.0), &
    sqrt(289.0)], [2, 2])) > 1e-5)) error stop 6

x = norm2(a, dim=1)
if (any(abs(x - [5.0, 10.0]) > 1e-5)) error stop 7
x = norm2(a, dim=2)
if (any(abs(x - [sqrt(45.0), sqrt(80.0)]) > 1e-5)) error stop 8
z = norm2(b, dim=1)
if (any(abs(z - [5.0d0, 10.0d0]) > 1d-12)) error stop 9
z = norm2(b, dim=2)
if (any(abs(z - [sqrt(45.0d0), sqrt(80.0d0)]) > 1d-12)) error stop 10
y = norm2(c, dim=1)
if (any(abs(y - reshape([5.0, 10.0, 13.0, 17.0], [2, 2])) > 1e-5)) error stop 11
y = norm2(c, dim=2)
if (any(abs(y - reshape([sqrt(45.0), sqrt(80.0), sqrt(89.0), &
    sqrt(369.0)], [2, 2])) > 1e-5)) error stop 12

cv = c
y = norm2(cv, dim=3)
if (any(abs(y - c3) > 1e-5)) error stop 13

if (any(q1 /= [5.0_16, 10.0_16])) error stop 14
print *, "ok"
end program
