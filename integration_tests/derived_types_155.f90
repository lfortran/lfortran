program derived_types_155
! Type parameter inquiries (%kind, %len) applied to a derived type component.
! These share the code path that resolves the trailing component of a
! multi-part designator, which used to assume the last component was always
! a member of a derived type.
implicit none
integer, parameter :: dp = kind(1.0d0)

type :: inner
    integer :: i
    real(dp) :: r
    character(len=5) :: s
end type inner

type :: outer
    type(inner) :: in
end type outer

type(inner) :: d
type(outer) :: o

d%i = 3
d%r = 1.5_dp
d%s = "abcde"

if (d%i%kind /= kind(d%i)) error stop
if (d%r%kind /= dp) error stop
if (d%s%len /= 5) error stop
if (d%s%kind /= kind("a")) error stop

o%in%s = "vwxyz"
if (o%in%s%len /= 5) error stop
if (o%in%r%kind /= dp) error stop

print *, "PASS"
end program derived_types_155
