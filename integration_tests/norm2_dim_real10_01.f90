program norm2_dim_real10_01
! norm2 with DIM of non-constant real(10) arrays (#17873)
implicit none
real(10) :: a(2,2), y(2), z(2)
real(10) :: b(2,2,2), w(2,2)
real(10), parameter :: tol = 1e-17_10
integer :: d

a = reshape([3.0_10, 4.0_10, 6.0_10, 8.0_10], [2,2])
y = norm2(a, dim=1)
z = norm2(a, dim=2)
if (any(abs(y - [5.0_10, 10.0_10]) > tol)) error stop 1
if (any(abs(z - [sqrt(45.0_10), sqrt(80.0_10)]) > tol)) error stop 2

d = 2
z = norm2(a, dim=d)
if (any(abs(z - [sqrt(45.0_10), sqrt(80.0_10)]) > tol)) error stop 3

b = reshape([3.0_10, 4.0_10, 6.0_10, 8.0_10, &
             4.0_10, 3.0_10, 8.0_10, 15.0_10], [2,2,2])
w = norm2(b, dim=3)
if (any(abs(w - reshape([5.0_10, 5.0_10, 10.0_10, 17.0_10], [2,2])) > tol)) &
    error stop 4
w = norm2(b, dim=1)
if (any(abs(w - reshape([5.0_10, 10.0_10, 5.0_10, 17.0_10], [2,2])) > tol)) &
    error stop 5
print *, "ok"
end program
