program dot_product_02
implicit none
real :: x(3)
integer, allocatable :: n(:)
real, allocatable :: y(:)
integer, allocatable :: m(:)
real :: r
type :: holder
    integer, allocatable :: n(:)
end type
type(holder) :: h

x = [1.5, 2.0, -1.0]
allocate(n(3))
n = [2, 3, 4]
r = dot_product(x, n)
if (r /= 5.0) error stop 1
r = dot_product(n, x)
if (r /= 5.0) error stop 2

allocate(y(3), m(3))
y = [1.5, 2.0, -1.0]
m = [2, 3, 4]
r = dot_product(y, m)
if (r /= 5.0) error stop 3
r = dot_product(m, y)
if (r /= 5.0) error stop 4

allocate(h%n(3))
h%n = [2, 3, 4]
r = dot_product(x, h%n)
if (r /= 5.0) error stop 5
r = dot_product(h%n, x)
if (r /= 5.0) error stop 6
end program
