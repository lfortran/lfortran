! Assigning a structure constructor gives an allocatable array component the
! bounds of its data source, also when the component is already allocated
! with the same shape and other bounds (#17226).
module structure_constructor_args_34_mod
implicit none
type :: holder
    real, allocatable :: d(:)
end type
type :: outer
    type(holder) :: in
    integer :: k = 0
end type
type :: matrix
    integer, allocatable :: m(:, :)
    real(8), allocatable :: d(:)
end type
type :: elem
    integer :: v = 0
end type
type :: wrap
    type(elem), allocatable :: e(:)
end type
type :: names
    character(len=3), allocatable :: s(:)
end type
contains

function fixed_result() result(r)
    real :: r(3)
    r = [7.0, 8.0, 9.0]
end function

function allocatable_result() result(r)
    real, allocatable :: r(:)
    allocate(r(5:7))
    r = [7.0, 8.0, 9.0]
end function

subroutine check(h, lb, ub, expected, code)
    type(holder), intent(in) :: h
    integer, intent(in) :: lb, ub, code
    real, intent(in) :: expected(:)
    if (.not. allocated(h%d)) error stop code
    if (lbound(h%d, 1) /= lb) error stop code
    if (ubound(h%d, 1) /= ub) error stop code
    if (any(h%d /= expected)) error stop code
end subroutine

subroutine dummy_sources(a, p, h)
    real, intent(in) :: a(5:)
    real, pointer, intent(in) :: p(:)
    type(holder), intent(inout) :: h
    h = holder(a)
    call check(h, 5, 7, [4.0, 5.0, 6.0], 30)
    h = holder(p)
    call check(h, 3, 5, [10.0, 11.0, 12.0], 31)
    h = holder(a(:))
    call check(h, 1, 3, [4.0, 5.0, 6.0], 32)
end subroutine

! The constructor is built in a temporary that is reused by every call
subroutine check_argument(h, lb, ub, expected, code)
    type(holder), intent(in) :: h
    integer, intent(in) :: lb, ub, code
    real, intent(in) :: expected(:)
    call check(h, lb, ub, expected, code)
end subroutine
end module

program structure_constructor_args_34
use structure_constructor_args_34_mod
implicit none
type(holder) :: h, g
type(holder) :: hs(2)
type(holder), allocatable :: ha
type(outer) :: o
type(matrix) :: mat
type(wrap) :: w
type(names) :: n
real :: vals(3), src(5:7)
real, allocatable :: a(:)
real, pointer :: p(:)
real, target :: tgt(-2:0)
integer :: im(0:1, 3:4), i
real(8) :: dv(2:3)
type(elem) :: es(4:5)
character(len=3) :: cs(0:1)

vals = [1.0, 2.0, 3.0]
src = [4.0, 5.0, 6.0]
tgt = [10.0, 11.0, 12.0]

! An array of the same shape and other bounds
allocate(h%d(5:7))
h%d = 0.0
h = holder(vals)
call check(h, 1, 3, [1.0, 2.0, 3.0], 1)
h = holder(src)
call check(h, 5, 7, [4.0, 5.0, 6.0], 2)

! A value that is not a whole array has lower bounds of 1
h = holder(src(:))
call check(h, 1, 3, [4.0, 5.0, 6.0], 3)
h = holder(src)
h = holder(vals + 1.0)
call check(h, 1, 3, [2.0, 3.0, 4.0], 4)
h = holder(src)
h = holder([1.0, 2.0, 3.0])
call check(h, 1, 3, [1.0, 2.0, 3.0], 5)
h = holder(src)
h = holder(fixed_result())
call check(h, 1, 3, [7.0, 8.0, 9.0], 6)
h = holder(src)
h = holder(allocatable_result())
call check(h, 1, 3, [7.0, 8.0, 9.0], 7)

! Allocatable, pointer and component data sources
allocate(a(0:2))
a = [1.0, 2.0, 3.0]
h = holder(a)
call check(h, 0, 2, [1.0, 2.0, 3.0], 8)
p => tgt
h = holder(p)
call check(h, -2, 0, [10.0, 11.0, 12.0], 9)
p(3:) => tgt
h = holder(p)
call check(h, 3, 5, [10.0, 11.0, 12.0], 10)
g = holder(vals)
h = holder(g%d)
call check(h, 1, 3, [1.0, 2.0, 3.0], 11)
g = holder(src)
h = holder(g%d)
call check(h, 5, 7, [4.0, 5.0, 6.0], 12)

! The bounds are kept when they are those of the data source
h = holder(src)
call check(h, 5, 7, [4.0, 5.0, 6.0], 13)
h = holder(vals)
h = holder(vals)
call check(h, 1, 3, [1.0, 2.0, 3.0], 14)

! Another shape
h = holder(src)
h = holder(src(6:7))
call check(h, 1, 2, [5.0, 6.0], 15)
h = holder(src)
call check(h, 5, 7, [4.0, 5.0, 6.0], 16)

! A data source that reads the component it is assigned to
h = holder(h%d)
call check(h, 5, 7, [4.0, 5.0, 6.0], 17)
h = holder(h%d(:))
call check(h, 1, 3, [4.0, 5.0, 6.0], 18)
h = holder(src)
h = holder(h%d + 1.0)
call check(h, 1, 3, [5.0, 6.0, 7.0], 19)

! An unallocated data source
deallocate(a)
h = holder(a)
if (allocated(h%d)) error stop 20

! A nested constructor
o = outer(holder(src), 1)
call check(o%in, 5, 7, [4.0, 5.0, 6.0], 21)
o = outer(holder(vals), 2)
call check(o%in, 1, 3, [1.0, 2.0, 3.0], 22)
if (o%k /= 2) error stop 23
o = outer(holder(src), 3)
o = outer(holder(src(:)), 4)
call check(o%in, 1, 3, [4.0, 5.0, 6.0], 24)

! An element of an array and an allocatable scalar
hs(1) = holder(src)
hs(2) = holder(src)
hs(2) = holder(vals)
call check(hs(1), 5, 7, [4.0, 5.0, 6.0], 25)
call check(hs(2), 1, 3, [1.0, 2.0, 3.0], 26)
ha = holder(src)
ha = holder(vals)
call check(ha, 1, 3, [1.0, 2.0, 3.0], 27)

call dummy_sources(src, p, h)

! A constructor that is an actual argument
do i = 1, 2
    if (i == 1) then
        allocate(a(5:7))
        a = 1.0
        call check_argument(holder(a), 5, 7, [1.0, 1.0, 1.0], 40)
    else
        deallocate(a)
        allocate(a(1:3))
        a = 2.0
        call check_argument(holder(a), 1, 3, [2.0, 2.0, 2.0], 41)
    end if
end do

! Two components, one of rank 2
im = reshape([1, 2, 3, 4], [2, 2])
dv = [1.0d0, 2.0d0]
mat = matrix(im, dv)
if (any(lbound(mat%m) /= [0, 3]) .or. any(ubound(mat%m) /= [1, 4])) error stop 50
if (lbound(mat%d, 1) /= 2 .or. ubound(mat%d, 1) /= 3) error stop 51
mat = matrix(im(:, :), dv(:))
if (any(lbound(mat%m) /= [1, 1]) .or. any(ubound(mat%m) /= [2, 2])) error stop 52
if (lbound(mat%d, 1) /= 1 .or. ubound(mat%d, 1) /= 2) error stop 53
if (any(mat%m /= im) .or. any(mat%d /= dv)) error stop 54
! Only the lower bound of the second dimension differs
mat = matrix(im, dv)
mat = matrix(im(0:1, :), dv)
if (any(lbound(mat%m) /= [1, 1]) .or. any(ubound(mat%m) /= [2, 2])) error stop 55
if (lbound(mat%d, 1) /= 2 .or. ubound(mat%d, 1) /= 3) error stop 56
if (any(mat%m /= im) .or. any(mat%d /= dv)) error stop 57

! Components of derived type and of character type
es(4)%v = 1
es(5)%v = 2
w = wrap(es)
if (lbound(w%e, 1) /= 4 .or. ubound(w%e, 1) /= 5) error stop 60
w = wrap(es(:))
if (lbound(w%e, 1) /= 1 .or. ubound(w%e, 1) /= 2) error stop 61
if (w%e(1)%v /= 1 .or. w%e(2)%v /= 2) error stop 62
cs = ["abc", "def"]
n = names(cs)
if (lbound(n%s, 1) /= 0 .or. ubound(n%s, 1) /= 1) error stop 63
n = names(cs(:))
if (lbound(n%s, 1) /= 1 .or. ubound(n%s, 1) /= 2) error stop 64
if (n%s(1) /= "abc" .or. n%s(2) /= "def") error stop 65

print *, "ok"
end program
