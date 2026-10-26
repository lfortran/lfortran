module array_size_section_01_mod
implicit none
contains

subroutine check_assumed(b, n)
    integer, intent(in) :: b(:, :)
    integer, intent(in) :: n
    integer :: d
    if (size(b(2:n:2, 1)) /= 2) error stop
    if (size(b(n:1:-3, :)) /= 8) error stop
    if (size(b(n:1:-3, :), 1) /= 2) error stop
    if (size(b(n:1:-3, :), 2) /= 4) error stop
    if (size(b(3:2, :)) /= 0) error stop
    if (size(b(3:2, :), 1) /= 0) error stop
    if (size(b(1:0:2, 2)) /= 0) error stop
    if (size(b(1:0:2, 2), 1) /= 0) error stop
    if (size(b(:, 3)) /= n) error stop
    d = 2
    if (size(b(1:n:2, 2:4), d) /= 3) error stop
    d = 1
    if (size(b(1:n:2, 2:4), d) /= 3) error stop
end subroutine

end module

program array_size_section_01
use array_size_section_01_mod, only: check_assumed
implicit none
integer :: a(10), m(5, 4), i, n, d
integer, allocatable :: al(:, :)
integer, pointer :: p(:)
integer, target :: t(12)

a = 0
m = 0
n = 5

! Strides and negative strides
if (size(a(1:10:3)) /= 4) error stop
if (size(a(10:1:-3)) /= 4) error stop
if (size(a(10:1:-3), 1) /= 4) error stop
if (size(a(2:9:4)) /= 2) error stop
if (size(a(9:2:-4), dim=1) /= 2) error stop
if (size(a(n:1:-1)) /= 5) error stop
if (size(a(1:n+3:n-3)) /= 4) error stop

! Empty extents, also when the stride is larger than the distance
if (size(a(5:4)) /= 0) error stop
if (size(a(5:4), 1) /= 0) error stop
if (size(a(1:5:-1)) /= 0) error stop
if (size(a(1:5:-1), 1) /= 0) error stop
if (size(a(1:0:2)) /= 0) error stop
if (size(a(1:0:2), 1) /= 0) error stop
if (size(a(2:1:-3)) /= 1) error stop
if (size(a(n:n-1)) /= 0) error stop

! Rank 2 with a scalar subscript, dim= present and absent
if (size(m(2, :)) /= 4) error stop
if (size(m(2, :), 1) /= 4) error stop
if (size(m(:, 3)) /= 5) error stop
if (size(m(1:5:2, 2:4)) /= 9) error stop
if (size(m(1:5:2, 2:4), 1) /= 3) error stop
if (size(m(1:5:2, 2:4), 2) /= 3) error stop
if (size(m(5:1:-2, 3)) /= 3) error stop
if (size(m(n, 4:1:-1), 1) /= 4) error stop
if (size(m(4:3, 2:4)) /= 0) error stop
if (size(m(4:3, 2:4), 2) /= 3) error stop

! dim= known only at run time
d = 1
if (size(m(1:5:2, 2:3), d) /= 3) error stop
d = 2
if (size(m(1:5:2, 2:3), d) /= 2) error stop
if (size(m(n, 1:4:2), d - 1) /= 2) error stop

! Allocatable
allocate(al(6, 7))
al = 0
if (size(al(2:6:2, :)) /= 21) error stop
if (size(al(6:1:-1, 3)) /= 6) error stop
if (size(al(:, 7:1:-2), 2) /= 4) error stop
if (size(al(3:2, :)) /= 0) error stop
d = 2
if (size(al(1:6:5, 2:7), d) /= 6) error stop

! Pointer
t = [(i, i = 1, 12)]
p => t(2:12:2)
if (size(p) /= 6) error stop
if (size(p(1:6:2)) /= 3) error stop
if (size(p(6:1:-4), 1) /= 2) error stop
if (size(p(4:3)) /= 0) error stop

! Assumed shape
call check_assumed(m, n)

print *, size(a(10:1:-3)), size(m(1:5:2, 2:4), 2), size(al(2:6:2, :)), &
    size(p(1:6:2))
end program
