! A deferred-shape array declared by a type declaration statement and given
! the ALLOCATABLE attribute by an ALLOCATABLE statement that follows
! (#15291) or precedes it
module allocatable_stmt_01_m
implicit none
real, dimension(:,:) :: g
allocatable :: g
contains
subroutine fill(n, x)
integer, intent(in) :: n
real, dimension(:) :: x
real, dimension(:) :: w
allocatable :: x, w
allocate(x(n), w(n))
x = 1
w = 2
x = x + w
end subroutine
end module

program allocatable_stmt_01
use allocatable_stmt_01_m
implicit none
allocatable :: c
real(8), dimension(:) :: a
integer, dimension(:) :: b
real, dimension(:) :: y
integer :: c(:)
allocatable :: a
allocatable b, y
allocate(a(3))
a = 1.5d0
if (.not. allocated(a)) error stop
if (abs(sum(a) - 4.5d0) > 1d-12) error stop
deallocate(a)
if (allocated(a)) error stop
allocate(b(4))
b = [1, 2, 3, 4]
if (size(b) /= 4) error stop
if (sum(b) /= 10) error stop
allocate(c(2))
c = 5
if (sum(c) /= 10) error stop
allocate(g(2,3))
g = 1
if (sum(g) /= 6) error stop
call fill(4, y)
if (size(y) /= 4) error stop
if (any(y /= 3)) error stop
block
    integer, dimension(:) :: k
    allocatable :: k
    allocate(k(5))
    k = 7
    if (sum(k) /= 35) error stop
end block
print *, "ok"
end program
