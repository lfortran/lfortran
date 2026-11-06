program data_implied_do_11
! DATA implied-DO objects that are components of array elements
implicit none
type :: u
    integer :: k
end type
type :: t
    real :: v
    type(u) :: w
    integer :: x(2)
end type
integer :: i, j
type(t) :: a(2), b(2, 2)
data (a(i)%v, i=1,2) / 1.0, 2.0 /
data (a(i)%w%k, i=1,2) / 3, 4 /
data ((a(i)%x(j), j=1,2), i=1,2) / 5, 6, 7, 8 /
data ((b(i, j)%w%k, i=1,2), j=1,2) / 11, 21, 12, 22 /
data (b(i, 1)%v, b(i, 2)%v, i=1,2) / 1.5, 2.5, 3.5, 4.5 /
if (a(1)%v /= 1.0) error stop
if (a(2)%v /= 2.0) error stop
if (a(1)%w%k /= 3) error stop
if (a(2)%w%k /= 4) error stop
if (any(a(1)%x /= [5, 6])) error stop
if (any(a(2)%x /= [7, 8])) error stop
if (b(1, 1)%w%k /= 11) error stop
if (b(2, 1)%w%k /= 21) error stop
if (b(1, 2)%w%k /= 12) error stop
if (b(2, 2)%w%k /= 22) error stop
if (b(1, 1)%v /= 1.5) error stop
if (b(1, 2)%v /= 2.5) error stop
if (b(2, 1)%v /= 3.5) error stop
if (b(2, 2)%v /= 4.5) error stop
call sub()
print *, a(1)%v, a(2)%v, a(1)%w%k, a(2)%w%k
contains
subroutine sub()
    type(t) :: c(3)
    data (c(i)%w%k, i=1,3,2) / 30, 31 /
    if (c(1)%w%k /= 30) error stop
    if (c(3)%w%k /= 31) error stop
end subroutine
end program
