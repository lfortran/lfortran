! A DIMENSION statement, a type declaration without the array-spec and an
! ALLOCATABLE attribute, in each order (#15291): the attribute belongs to the
! array, not to its elements.
module allocatable_stmt_03_m
implicit none
dimension g(:,:)
integer g
allocatable g
contains
subroutine fill(d)
dimension d(:)
real :: d
allocatable d
allocate(d(2))
d = 3
end subroutine
end module

program allocatable_stmt_03
use allocatable_stmt_03_m
implicit none
dimension a(:)
real :: a
allocatable a
allocatable b
dimension b(:)
real :: b
dimension c(:)
real, allocatable :: c
real, allocatable :: x(:)
allocate(a(2))
a = 3
if (sum(a) /= 6) error stop
allocate(b(2))
b = 4
if (sum(b) /= 8) error stop
allocate(c(2))
c = 5
if (sum(c) /= 10) error stop
allocate(g(2,2))
g = 1
if (sum(g) /= 4) error stop
call fill(x)
if (size(x) /= 2) error stop
if (sum(x) /= 6) error stop
print *, "ok"
end program
