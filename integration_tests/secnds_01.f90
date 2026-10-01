program secnds_01
! SECNDS(X) (GNU and Intel extension): seconds since local midnight minus X,
! used without a declaration, as an intrinsic
implicit none
real :: t0, d1, d2

t0 = secnds(0.0)
print *, "secnds(0.0) = ", t0
if (t0 < 0.0 .or. t0 > 86400.0) error stop "secnds(0.0) out of range"

! one second before t0: about one second has elapsed (wraps past midnight)
d1 = secnds(t0 - 1.0)
d2 = secnds(real(t0 - 1.0, 4))
print *, "elapsed = ", d1, d2
if (d1 < 0.99 .or. d1 > 61.0) error stop "secnds(t0 - 1.0) is not the elapsed time"
if (d2 < d1) error stop "secnds went backwards"
end program secnds_01
