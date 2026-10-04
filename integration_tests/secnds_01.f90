program secnds_01
! SECNDS(X) (GNU and Intel extension): seconds since local midnight minus X,
! used without a declaration, as an intrinsic
implicit none
real :: t0, t1, d1, d2, d3, d4

t0 = secnds(0.0)
print *, "secnds(0.0) = ", t0
if (t0 < 0.0 .or. t0 > 86400.0) error stop "secnds(0.0) out of range"

! one second before t0: about one second has elapsed (wraps past midnight)
d1 = secnds(t0 - 1.0)
d2 = secnds(real(t0 - 1.0, 4))
print *, "elapsed = ", d1, d2
if (d1 < 0.99 .or. d1 > 61.0) error stop "secnds(t0 - 1.0) is not the elapsed time"
if (d2 < d1) error stop "secnds went backwards"

! the clock read right after T0 is not before it: no wrap past midnight
d3 = secnds(secnds(0.0))
print *, "secnds(secnds(0.0)) = ", d3
if (d3 < 0.0 .or. d3 > 61.0) error stop "secnds(secnds(0.0)) wrapped"

! a negative X is reduced with fmod and keeps its sign, as in GFortran
t1 = secnds(0.0)
d4 = secnds(-86399.0)
print *, "secnds(-86399.0) - t1 = ", d4 - t1
if (t1 < 86000.0) then
    if (d4 - t1 < 86398.5 .or. d4 - t1 > 86460.0) error stop "secnds(-86399.0)"
end if
end program secnds_01
