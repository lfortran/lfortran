module parameter_32_mod
implicit none
type :: ic
    integer :: k = 0
end type
type :: outer
    type(ic) :: inner
    integer :: n
    integer :: arr(3)
    type(ic) :: icarr(2)
end type
type(outer), parameter :: o = outer(ic(5), 7, [1, 2, 3], [ic(8), ic(9)])
type(ic) :: mw = o%inner
type(ic), parameter :: pw = o%inner
integer :: mk = o%n
integer, parameter :: pn = o%inner%k
integer :: ma(3) = o%arr
type(ic) :: mica(2) = o%icarr
end module

program parameter_32
use parameter_32_mod
implicit none
type :: a_t
    integer :: b = 1
end type
type :: mid_t
    type(a_t) :: a
end type
type :: top_t
    type(mid_t) :: m
end type
type(top_t), parameter :: co = top_t(mid_t(a_t(42)))
type(a_t) :: x = co%m%a
type(mid_t) :: y = co%m
type(a_t), parameter :: z = co%m%a
type(ic) :: w = o%inner
type(ic), parameter :: lp = o%inner
type(ic) :: wa(2) = o%icarr
type(ic) :: we = o%icarr(2)
integer :: k = o%inner%k
integer :: a(o%n)
if (w%k /= 5) error stop 1
if (lp%k /= 5) error stop 2
if (mw%k /= 5) error stop 3
if (pw%k /= 5) error stop 4
if (mk /= 7) error stop 5
if (pn /= 5) error stop 6
if (any(ma /= [1, 2, 3])) error stop 7
if (mica(1)%k /= 8 .or. mica(2)%k /= 9) error stop 8
if (wa(1)%k /= 8 .or. wa(2)%k /= 9) error stop 9
if (we%k /= 9) error stop 10
if (k /= 5) error stop 11
if (size(a) /= 7) error stop 12
if (x%b /= 42) error stop 13
if (y%a%b /= 42) error stop 14
if (z%b /= 42) error stop 15
call sub()
print *, "ok"
contains
subroutine sub()
type(ic) :: sw = o%inner
type(a_t) :: sx = co%m%a
if (sw%k /= 5) error stop 20
if (sx%b /= 42) error stop 21
end subroutine
end program
