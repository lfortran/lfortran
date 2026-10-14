! Named constants assigned to array sections, with and without a block copy
module arrays_op_33_m
implicit none
real :: a(8, 4)
real, parameter :: c(8) = [1., 2., 3., 4., 5., 6., 7., 8.]
type :: t
    integer :: k(3, 2)
end type
contains

subroutine fill_explicit(x, n, m, j)
    integer, intent(in) :: n, m, j
    real, intent(inout) :: x(n, m)
    x(1:3, j) = [7., 8., 9.]
end subroutine

subroutine fill_assumed(x, j)
    real, intent(inout) :: x(:, :)
    integer, intent(in) :: j
    x(:, j) = [4., 5.]
end subroutine

end module

program arrays_op_33
use arrays_op_33_m
implicit none
real(8) :: d(10, 3)
integer :: v(6)
complex :: z(2, 2)
logical :: l(3, 2)
character(2) :: s(2, 2)
type(t) :: ts(2)
real :: e(8, 3), big(8, 4), w(5, 2)
real, allocatable :: r(:, :)
real, pointer :: p(:, :)
real, target :: tgt(8, 4)
integer, parameter :: ip(3) = [1, 2, 3]
complex, parameter :: pz(2) = [(1., 2.), (3., 4.)]
logical, parameter :: pl(3) = [.true., .false., .true.]
integer :: i, j

a = 0
a(:, 1) = [1., 2., 3., 4., 5., 6., 7., 8.]
i = 3
a(:, i) = c
a(1:8, 4) = c
do j = 1, 8
    if (a(j, 1) /= real(j) .or. a(j, 3) /= real(j) .or. a(j, 4) /= real(j)) error stop
    if (a(j, 2) /= 0) error stop
end do

! Part of the first dimension, from an offset
d = -1
d(2:9, 2) = [1d0, 2d0, 3d0, 4d0, 5d0, 6d0, 7d0, 8d0]
if (d(1, 2) /= -1 .or. d(10, 2) /= -1 .or. any(d(:, 1) /= -1)) error stop
if (any(d(:, 3) /= -1)) error stop
do j = 2, 9
    if (d(j, 2) /= j - 1) error stop
end do

! Rank one
v = 0
v(3:5) = [7, 8, 9]
if (any(v /= [0, 0, 7, 8, 9, 0])) error stop

! Complex and logical
z = 0
z(:, 2) = pz
if (any(z(:, 1) /= 0) .or. z(1, 2) /= (1., 2.) .or. z(2, 2) /= (3., 4.)) error stop
l = .false.
l(:, 2) = pl
if (any(l(:, 1)) .or. .not. l(1, 2) .or. l(2, 2) .or. .not. l(3, 2)) error stop

! Character elements are assigned element by element
s = "zz"
s(:, 1) = ["ab", "cd"]
if (s(1, 1) /= "ab" .or. s(2, 1) /= "cd" .or. any(s(:, 2) /= "zz")) error stop

! Components of derived types
do j = 1, 2
    ts(j)%k = 0
end do
ts(2)%k(:, 1) = [4, 5, 6]
if (any(ts(2)%k(:, 1) /= [4, 5, 6]) .or. any(ts(2)%k(:, 2) /= 0)) error stop
if (any(ts(1)%k /= 0)) error stop

! A conversion from integer is assigned element by element
e = 0
e(1:3, 2) = ip
if (any(e(1:3, 2) /= [1., 2., 3.]) .or. any(e(4:8, 2) /= 0)) error stop

! Allocatable target
allocate(r(4, 3))
r = 0
r(1:4, 2) = [1., 2., 3., 4.]
r(2:3, 3) = [5., 6.]
if (any(r(:, 1) /= 0) .or. any(r(:, 2) /= [1., 2., 3., 4.])) error stop
if (any(r(:, 3) /= [0., 5., 6., 0.])) error stop

! A pointer may be associated with a strided section
tgt = 0
p => tgt(1:8:2, :)
p(:, 2) = [1., 2., 3., 4.]
if (any(tgt(:, 2) /= [1., 0., 2., 0., 3., 0., 4., 0.])) error stop
if (any(tgt(:, 1) /= 0) .or. any(tgt(:, 3:4) /= 0)) error stop

! Explicit-shape and assumed-shape dummy arguments
big = 0
call fill_explicit(big, 8, 4, 3)
if (any(big(1:3, 3) /= [7., 8., 9.]) .or. any(big(4:8, 3) /= 0)) error stop
w = 0
call fill_assumed(w(1:3:2, :), 2)
if (any(w(:, 2) /= [4., 0., 5., 0., 0.]) .or. any(w(:, 1) /= 0)) error stop
print *, "ok"
end program
