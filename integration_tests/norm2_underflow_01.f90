program norm2_underflow_01
! norm2 must not underflow at run time when the result is representable,
! even if the squares of the elements underflow (#17877), with and without
! dim. GFortran's run-time norm2 returns 0 for these inputs, so this test
! is not run with GFortran; the folded cases are in norm2_overflow_01.
use iso_c_binding, only: c_long_double
implicit none
integer, parameter :: k10 = merge(10, 16, c_long_double == 10)
real(4), parameter :: tol4 = 4*epsilon(1.0_4)
real(8), parameter :: tol8 = 4*epsilon(1.0_8)
real(k10), parameter :: tol10 = 4*epsilon(1.0_k10)
real(16), parameter :: tol16 = 4*epsilon(1.0_16)
real(4) :: s(2), s2(2, 2), r4(2)
real(8) :: h(3), h2(2, 2), r8(2)
real(k10) :: e(2)
real(16) :: q(2)
integer :: d

s = [3.0e-30_4, 4.0e-30_4]
if (abs(norm2(s) - 5.0e-30_4) > tol4*5.0e-30_4) error stop 1

h = [3.0e-200_8, 4.0e-200_8, 0.0_8]
if (abs(norm2(h) - 5.0e-200_8) > tol8*5.0e-200_8) error stop 2

e = [3.0e-4000_k10, 4.0e-4000_k10]
if (abs(norm2(e) - 5.0e-4000_k10) > tol10*5.0e-4000_k10) error stop 3

q = [3.0e-4000_16, 4.0e-4000_16]
if (abs(norm2(q) - 5.0e-4000_16) > tol16*5.0e-4000_16) error stop 4

! The smallest normal number is returned exactly
h = [tiny(1.0_8), 0.0_8, 0.0_8]
if (norm2(h) /= tiny(1.0_8)) error stop 5

! With dim, constant and variable
h2 = reshape([3.0e-200_8, 4.0e-200_8, 4.0e-200_8, 3.0e-200_8], [2, 2])
r8 = norm2(h2, dim=1)
if (any(abs(r8 - 5.0e-200_8) > tol8*5.0e-200_8)) error stop 6
r8 = norm2(h2, dim=2)
if (any(abs(r8 - 5.0e-200_8) > tol8*5.0e-200_8)) error stop 7
d = 2
r8 = norm2(h2, dim=d)
if (any(abs(r8 - 5.0e-200_8) > tol8*5.0e-200_8)) error stop 8
s2 = reshape([3.0e-30_4, 4.0e-30_4, 4.0e-30_4, 3.0e-30_4], [2, 2])
r4 = norm2(s2, dim=1)
if (any(abs(r4 - 5.0e-30_4) > tol4*5.0e-30_4)) error stop 9

print *, norm2(s), norm2(h), norm2(e), norm2(q), r8, r4
end program
