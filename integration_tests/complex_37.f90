program complex_37
! Test %re and %im on allocatable (deferred-shape) complex arrays.
! This used to crash in LLVM codegen because the real-part view's
! descriptor was built from compile-time dims that a deferred-shape
! array does not have.
implicit none
integer, parameter :: sp = kind(1.0), dp = kind(1.0d0)
complex(sp), allocatable :: z(:)
complex(dp), allocatable :: w(:)
real(sp) :: re4(3), im4(3)
real(dp) :: im8(2)
integer :: i

allocate(z(3))
do i = 1, 3
    z(i) = cmplx(real(i, sp), real(10*i, sp), kind=sp)
end do

re4 = z%re
im4 = z%im
do i = 1, 3
    if (abs(re4(i) - real(i, sp)) > 1e-6_sp) error stop
    if (abs(im4(i) - real(10*i, sp)) > 1e-6_sp) error stop
end do

print "(A,3F9.1)", 'z%im =', z%im
print "(A,3F9.1)", 'z%re =', z%re

allocate(w(2))
w(1) = (1.5_dp, -2.5_dp)
w(2) = (3.5_dp, -4.5_dp)
im8 = w%im
if (abs(im8(1) + 2.5_dp) > 1e-14_dp) error stop
if (abs(im8(2) + 4.5_dp) > 1e-14_dp) error stop
if (abs(sum(w%re) - 5.0_dp) > 1e-14_dp) error stop

print *, "PASS"
end program
