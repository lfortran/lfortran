! An internal procedure uses an assumed-size dummy of derived type of its
! host, with and without allocatable components, through the bounds the host
! declares. The host neither initializes nor finalizes the elements, which
! belong to the actual argument. The same for a bind(c) host.
module host_assoc_15_m
use iso_c_binding, only: c_int
implicit none
type :: t
    integer :: i
end type
type :: u
    integer :: i
    integer, allocatable :: v(:)
end type
type, bind(c) :: pt
    integer(c_int) :: x, y
end type
contains
subroutine s1(x, k)
type(t) :: x(*)
integer :: k
call inner()
contains
subroutine inner()
x(k)%i = x(k - 1)%i + 40
end subroutine
end subroutine
subroutine s2(y, n, total)
integer :: n, total
type(u) :: y(0:n, *)
call inner()
contains
subroutine inner()
total = sum(y(1, 2)%v) + y(0, 1)%i
y(0, 2)%i = size(y(1, 2)%v)
allocate(y(1, 1)%v(3))
y(1, 1)%v = [7, 8, 9]
end subroutine
end subroutine
subroutine s3(r, n, s) bind(c)
integer(c_int) :: n, s
type(pt) :: r(*)
call inner()
contains
subroutine inner()
s = r(n)%x * r(n)%y
r(n)%y = -1
end subroutine
end subroutine
end module

program host_assoc_15
use host_assoc_15_m
implicit none
type(t) :: a(3)
type(u) :: b(2, 2)
integer :: total
type(pt) :: c(2)
a(:)%i = [1, 2, 3]
call s1(a, 2)
if (a(1)%i /= 1) error stop 1
if (a(2)%i /= 41) error stop 2
if (a(3)%i /= 3) error stop 3
b(:, :)%i = 0
b(1, 1)%i = 5
allocate(b(2, 2)%v(4))
b(2, 2)%v = [1, 2, 3, 4]
call s2(b, 1, total)
if (total /= 15) error stop 4
if (b(1, 2)%i /= 4) error stop 5
if (.not. allocated(b(2, 2)%v)) error stop 6
if (any(b(2, 2)%v /= [1, 2, 3, 4])) error stop 7
if (.not. allocated(b(2, 1)%v)) error stop 8
if (any(b(2, 1)%v /= [7, 8, 9])) error stop 9
if (allocated(b(1, 1)%v) .or. allocated(b(1, 2)%v)) error stop 10
c = [pt(1, 2), pt(3, 4)]
call s3(c, 2, total)
if (total /= 12 .or. c(2)%y /= -1 .or. c(1)%y /= 2) error stop 11
print *, a(:)%i, total
end program
