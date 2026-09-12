program complex_38
! Complex part designators (%re, %im) applied to a derived type component.
! Resolving the trailing `%re` of `d%c%re` used to crash the compiler because
! the last component of a multi-part designator was always assumed to be a
! member of a derived type.
implicit none
integer, parameter :: dp = kind(1.0d0)

type :: inner
    complex :: c
    complex(dp) :: c8
    complex :: ca(3)
end type inner

type :: outer
    type(inner) :: i
end type outer

type(inner) :: d
type(outer) :: o
real :: r(3)

d%c = (1.0, 2.0)

! read a component's real and imaginary part
if (abs(d%c%re - 1.0) > 1e-6) error stop
if (abs(d%c%im - 2.0) > 1e-6) error stop

! assign to a component's real and imaginary part
d%c%re = 5.0
if (abs(d%c%re - 5.0) > 1e-6) error stop
if (abs(d%c%im - 2.0) > 1e-6) error stop
d%c%im = -3.0
if (abs(d%c%re - 5.0) > 1e-6) error stop
if (abs(d%c%im + 3.0) > 1e-6) error stop

! pass a component's part as an INTENT(INOUT) actual argument
call double_it(d%c%re)
if (abs(d%c%re - 10.0) > 1e-6) error stop
if (abs(d%c%im + 3.0) > 1e-6) error stop
call double_it(d%c%im)
if (abs(d%c%re - 10.0) > 1e-6) error stop
if (abs(d%c%im + 6.0) > 1e-6) error stop

! double precision component
d%c8 = (1.5_dp, -2.5_dp)
if (abs(d%c8%re - 1.5_dp) > 1e-14_dp) error stop
d%c8%im = 4.5_dp
if (abs(d%c8%im - 4.5_dp) > 1e-14_dp) error stop
if (abs(d%c8%re - 1.5_dp) > 1e-14_dp) error stop

! complex array component
d%ca = (7.0, 8.0)
r = d%ca%re
if (any(abs(r - 7.0) > 1e-6)) error stop
if (any(abs(d%ca%im - 8.0) > 1e-6)) error stop

! nested derived type
o%i%c = (11.0, 12.0)
if (abs(o%i%c%re - 11.0) > 1e-6) error stop
o%i%c%re = 13.0
if (abs(o%i%c%re - 13.0) > 1e-6) error stop
if (abs(o%i%c%im - 12.0) > 1e-6) error stop
call double_it(o%i%c%im)
if (abs(o%i%c%im - 24.0) > 1e-6) error stop

! %kind on a complex component
if (d%c%kind /= kind(1.0)) error stop
if (d%c8%kind /= dp) error stop

print *, "PASS"

contains

subroutine double_it(r)
real, intent(inout) :: r
    r = r*2
end subroutine double_it

end program complex_38
