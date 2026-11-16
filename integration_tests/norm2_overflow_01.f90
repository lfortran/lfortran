program norm2_overflow_01
! norm2 must not overflow or underflow when the result is representable,
! even if the squares of the elements are not (#17877): at run time, with
! and without dim, and when folded.
use iso_c_binding, only: c_long_double
implicit none
! real(10) exists only where the C long double kind is 10 (Linux x86-64);
! elsewhere the same checks run in real(16).
integer, parameter :: k10 = merge(10, 16, c_long_double == 10)
real(4), parameter :: tol4 = 4*epsilon(1.0_4)
real(8), parameter :: tol8 = 4*epsilon(1.0_8)
real(k10), parameter :: tol10 = 4*epsilon(1.0_k10)
real(16), parameter :: tol16 = 4*epsilon(1.0_16)

real(4), parameter :: p4_big = norm2([3.0e30_4, 4.0e30_4])
real(4), parameter :: p4_small = norm2([3.0e-30_4, 4.0e-30_4])
real(8), parameter :: p8_big = norm2([1.0e200_8, 1.0e200_8])
real(8), parameter :: p8_small = norm2([1.0e-200_8, 1.0e-200_8])
real(8), parameter :: p8_big2 = norm2(reshape([3.0e200_8, 4.0e200_8, &
    0.0_8, 0.0_8], [2, 2]))
real(16), parameter :: p16_big = norm2([3.0e4000_16, 4.0e4000_16])
real(16), parameter :: p16_small = norm2([3.0e-4000_16, 4.0e-4000_16])

real(4) :: s(2)
real(8) :: h(2), h2(2, 2), r8(2), n8(2)
real(k10) :: e(2)
real(16) :: q(2)
integer :: d

! Folded
if (abs(p4_big - 5.0e30_4) > tol4*5.0e30_4) error stop 1
if (abs(p4_small - 5.0e-30_4) > tol4*5.0e-30_4) error stop 2
if (abs(p8_big - sqrt(2.0_8)*1.0e200_8) > tol8*sqrt(2.0_8)*1.0e200_8) error stop 3
if (abs(p8_small - sqrt(2.0_8)*1.0e-200_8) > tol8*sqrt(2.0_8)*1.0e-200_8) error stop 4
if (abs(p8_big2 - 5.0e200_8) > tol8*5.0e200_8) error stop 5
if (abs(p16_big - 5.0e4000_16) > tol16*5.0e4000_16) error stop 6
if (abs(p16_small - 5.0e-4000_16) > tol16*5.0e-4000_16) error stop 7

! At run time
s = [3.0e30_4, 4.0e30_4]
if (abs(norm2(s) - 5.0e30_4) > tol4*5.0e30_4) error stop 8

h = [1.0e200_8, 1.0e200_8]
if (abs(norm2(h) - sqrt(2.0_8)*1.0e200_8) > tol8*sqrt(2.0_8)*1.0e200_8) error stop 9
h = [3.0e200_8, 4.0e200_8]
if (abs(norm2(h) - 5.0e200_8) > tol8*5.0e200_8) error stop 10

e = [3.0e4000_k10, 4.0e4000_k10]
if (abs(norm2(e) - 5.0e4000_k10) > tol10*5.0e4000_k10) error stop 11

q = [3.0e4000_16, 4.0e4000_16]
if (abs(norm2(q) - 5.0e4000_16) > tol16*5.0e4000_16) error stop 12
q = [huge(1.0_16), 1.0_16]
if (abs(norm2(q) - huge(1.0_16)) > tol16*huge(1.0_16)) error stop 13

! With dim, constant and variable
h2 = reshape([3.0e200_8, 4.0e200_8, 4.0e200_8, 3.0e200_8], [2, 2])
r8 = norm2(h2, dim=1)
if (any(abs(r8 - 5.0e200_8) > tol8*5.0e200_8)) error stop 14
r8 = norm2(h2, dim=2)
if (any(abs(r8 - 5.0e200_8) > tol8*5.0e200_8)) error stop 15
d = 1
r8 = norm2(h2, dim=d)
if (any(abs(r8 - 5.0e200_8) > tol8*5.0e200_8)) error stop 16

! The normal range is unaffected
h = [3.0_8, 4.0_8]
if (norm2(h) /= 5.0_8) error stop 17
h = [1.0_8, 1.0_8]
if (norm2(h) /= sqrt(2.0_8)) error stop 18
h2 = reshape([3.0_8, 4.0_8, 6.0_8, 8.0_8], [2, 2])
n8 = norm2(h2, dim=1)
if (any(n8 /= [5.0_8, 10.0_8])) error stop 19

print *, p4_big, p4_small, p8_big, p8_small, p16_big, p16_small
print *, norm2(s), norm2(e), norm2(q), r8
end program
