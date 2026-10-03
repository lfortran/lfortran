module derived_types_177_m
implicit none

type :: base_t
    integer :: x = 2
end type base_t

type, extends(base_t) :: mid_t
    integer :: y = 3
end type mid_t

type, extends(mid_t) :: top_t
    integer :: z = 4
end type top_t

type(mid_t), parameter :: pc = mid_t(22, 32)

end module derived_types_177_m

program derived_types_177
use derived_types_177_m
implicit none

type(mid_t), parameter :: pl = mid_t(22, 32)
type(top_t) :: t
character(len=64) :: s
integer :: a, b, c, ios

t = top_t(11, 22, 33)

! List-directed output of a whole object of an extended type prints the
! inherited components first, then the components of the extended type.
print *, pc
print *, t

! The same values, captured through an internal write, so that the number of
! written components, their order and their values can all be checked.
write(s, *) pl
read(s, *, iostat=ios) a, b
if (ios /= 0) error stop "output of pl is missing the inherited component"
if (a /= 22) error stop "wrong inherited component of pl"
if (b /= 32) error stop "wrong component of pl"

write(s, *) t
read(s, *, iostat=ios) a, b, c
if (ios /= 0) error stop "output of t is missing the inherited components"
if (a /= 11) error stop "wrong grandparent component of t"
if (b /= 22) error stop "wrong parent component of t"
if (c /= 33) error stop "wrong component of t"

! List-directed input fills the inherited components too.
s = " 7 8 9 "
read(s, *, iostat=ios) t
if (ios /= 0) error stop "input did not fill all the components of t"
if (t%x /= 7) error stop "wrong grandparent component read into t"
if (t%y /= 8) error stop "wrong parent component read into t"
if (t%z /= 9) error stop "wrong component read into t"

if (pc%x /= 22 .or. pc%y /= 32) error stop "wrong components of the parameter"

end program derived_types_177
