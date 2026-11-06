! A structure constructor that gives an allocatable component a value is not
! a constant (#13383), also when it is an element of an array constructor for
! an array component, directly, through an inherited component, or nested.
program structure_constructor_args_33
implicit none
type :: inner
    integer, allocatable :: a
end type
type :: outer
    type(inner) :: b(1)
end type
type :: base
    integer, allocatable :: p(:)
end type
type, extends(base) :: child
    integer :: n = 3
end type
type :: holder
    type(child) :: c(2)
end type
type :: wrap
    type(inner) :: w
end type
type :: top
    type(wrap) :: d(2)
end type
type :: plain
    integer, allocatable :: a
    integer :: n = 0
end type
type :: plain_holder
    type(plain) :: e(2)
end type
type(outer) :: x
type(holder) :: h
type(top) :: t
type(plain_holder) :: ph

x = outer([inner(5)])
if (.not. allocated(x%b(1)%a)) error stop
if (x%b(1)%a /= 5) error stop

h = holder([child([1, 2], 7), child([3], 8)])
if (size(h%c(1)%p) /= 2) error stop
if (any(h%c(1)%p /= [1, 2])) error stop
if (size(h%c(2)%p) /= 1) error stop
if (h%c(2)%p(1) /= 3) error stop
if (h%c(1)%n /= 7 .or. h%c(2)%n /= 8) error stop

t = top([wrap(inner(5)), wrap(inner(6))])
if (t%d(1)%w%a /= 5) error stop
if (t%d(2)%w%a /= 6) error stop

! An omitted allocatable component keeps the constructor constant.
ph = plain_holder([plain(n=7), plain(n=8)])
if (allocated(ph%e(1)%a) .or. allocated(ph%e(2)%a)) error stop
if (ph%e(1)%n /= 7 .or. ph%e(2)%n /= 8) error stop
print *, "ok"
end program
